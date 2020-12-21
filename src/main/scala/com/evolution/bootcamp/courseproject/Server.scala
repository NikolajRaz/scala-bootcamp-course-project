package com.evolution.bootcamp.courseproject

import java.util.UUID

import cats.effect.{ExitCode, IO, IOApp}
import com.evolution.bootcamp.courseproject.models.{
  Bet,
  BETS_CLOSED,
  BETS_OPEN,
  Black,
  Number,
  Phase,
  PLACE_BET,
  Player,
  Red,
  REMOVE_BET,
  RESULT_ANNOUNCED
}
import com.evolution.bootcamp.courseproject.models.Messages.{
  ErrorMessage,
  FromClient,
  PhaseUpdate,
  ResultMessage,
  ToClient,
  WarnMessage
}
import com.evolution.bootcamp.courseproject.ref.{Cache, Game}
import fs2.{Pipe, Stream}
import fs2.concurrent.{Queue, Topic}
import io.circe.parser._
import io.circe.syntax._
import org.http4s._
import org.http4s.dsl.io._
import org.http4s.implicits._
import org.http4s.server.blaze.BlazeServerBuilder
import org.http4s.server.websocket.WebSocketBuilder
import org.http4s.websocket.WebSocketFrame

import scala.concurrent.duration._
import scala.concurrent.ExecutionContext

//websocat "ws://127.0.0.1:9002/roulette"
//{"place": { "PLACE_BET": {}}, "placedScores": "10", "betType": "Re", "placedNumbers": []}
//{"place": { "REMOVE_BET": {}}, "placedScores": "10", "betType": "Re", "placedNumbers": []}
//{"place": { "PLACE_BET": {}}, "placedScores": "10", "betType": "Ev", "placedNumbers": []}
//{"place": { "PLACE_BET": {}}, "placedScores": "10", "betType": "Si", "placedNumbers": [1]}
//{"place": { "PLACE_BET": {}}, "placedScores": "10", "betType": "Bl", "placedNumbers": []}

object Server extends IOApp {
  val defaultScores: Long = 100

  def roulette(implicit topic: Topic[IO, WebSocketFrame],
               cacheOfPlayers: Cache[IO, UUID, Player],
               game: Game): HttpRoutes[IO] =
    HttpRoutes.of[IO] {
      case req @ GET -> Root / "roulette" =>
        //Here we process client messages
        def echoPipe(id: UUID): Pipe[IO, WebSocketFrame, WebSocketFrame] = {
          _.collect {
            case WebSocketFrame.Text(message, _) => {
              val json = decode[FromClient](message) match {
                case Right(value) => inspectPlaceBet(id, value)
                case Left(error) =>
                  IO(ErrorMessage(error.toString).asJson.toString)
              }
              for {
                message <- json
                response = WebSocketFrame.Text(message)
              } yield response
            }
          }.evalMap(text => text)
        }

        //Here we process messages from the Game, and the sending it to the client
        def generalPipe(
          id: UUID,
        ): Pipe[IO, WebSocketFrame, WebSocketFrame] = {
          _.collect {
            case WebSocketFrame.Text(message, _) =>
              val json = decode[PhaseUpdate](message) match {
                case Right(value) => processGameMessage(value, id)
                case Left(error) =>
                  IO(ErrorMessage(error.toString).asJson.toString)
              }
              for {
                message <- json
                response = WebSocketFrame.Text(message)
              } yield response

          }.evalMap(text => text)
        }

        for {
          queue <- Queue.unbounded[IO, WebSocketFrame]
          id = UUID.randomUUID()
          _ <- cacheOfPlayers.put(
            id,
            models.Player(defaultScores, List.empty, 0, queue)
          )
          combinedStream = Stream(
            queue.dequeue.through(echoPipe(id)),
            topic.subscribe(100).through(generalPipe(id))
          ).parJoinUnbounded
          response <- WebSocketBuilder[IO]
            .build(receive = queue.enqueue, send = combinedStream)
        } yield response
    }

  private def inspectPlaceBet(id: UUID, fromClient: FromClient)(
    implicit cacheOfPlayers: Cache[IO, UUID, Player],
    game: Game
  ): IO[String] = {
    for {
      gamePhase <- game.getGamePhase
      result <- gamePhase match {
        case BETS_OPEN =>
          toEitherList(
            fromClient.placedNumbers
              .map(x => Number.of(x))
          ) match {
            case Right(x) =>
              Bet.of(fromClient.betType, x, fromClient.placedScores) match {
                case Right(value) =>
                  fromClient.place match {
                    case PLACE_BET  => checkBalance(id, value, fromClient)
                    case REMOVE_BET => checkBet(id, value, fromClient)
                  }
                case Left(error) => IO(ErrorMessage(error).asJson.toString)
              }
            case _ =>
              IO(
                ErrorMessage("Error: Incorrect numbers format").asJson.toString
              )
          }
        case _ =>
          IO(
            WarnMessage("Warn: You can't place bets in this game phase!").asJson.toString
          )
      }
    } yield result
  }

  private def checkBalance(id: UUID, bet: Bet, fromClient: FromClient)(
    implicit cacheOfPlayers: Cache[IO, UUID, Player],
    game: Game
  ): IO[String] = {
    for {
      player <- cacheOfPlayers.get(id)
      placedScores = fromClient.placedScores
      status <- player match {
        case Some(p) if p.scores >= placedScores =>
          placeBet(id, bet, p, placedScores)
        case Some(p) =>
          IO(
            ErrorMessage(s"Not enough scores on your wallet - ${p.scores}").asJson.toString
          )
        case None => IO(ErrorMessage("Can't find user").asJson.toString)
      }
    } yield status
  }

  private def checkBet(id: UUID, bet: Bet, fromClient: FromClient)(
    implicit cacheOfPlayers: Cache[IO, UUID, Player],
    game: Game
  ): IO[String] = {
    for {
      player <- cacheOfPlayers.get(id)
      number <- game.get
      status <- player match {
        case Some(p) => removeBet(id, p, bet, number)

        case None => IO(ErrorMessage("Can't find user").asJson.toString)
      }
    } yield status
  }

  def removeBet(id: UUID, player: Player, bet: Bet, number: Number)(
    implicit cacheOfPlayers: Cache[IO, UUID, Player],
    game: Game
  ): IO[String] = {
    for {
      contains <- IO(player.bets.contains(bet))
      message <- contains match {
        case true =>
          for {
            numberOfRepeatedBet <- IO(player.bets.filterNot(x => x == bet).size)
            newList <- IO(makeListOfRepeatedBets(numberOfRepeatedBet, bet))
            _ <- cacheOfPlayers.update(
              id,
              Player(
                player.scores + bet.placedScores,
                player.bets.filter(x => x == bet) ++ newList,
                player.winningScores - bet.getResult(number),
                player.connection
              )
            )
            message <- IO(
              ToClient(
                BETS_OPEN,
                player.scores + bet.placedScores,
                "Bet removed"
              ).asJson.toString
            )
          } yield message
        case false =>
          IO(
            WarnMessage("You did not placed anything in this round").asJson.toString
          )
      }
    } yield message
  }

  def makeListOfRepeatedBets(n: Int, bet: Bet): List[Bet] = {
    if (n > 0)
      List(bet) ++ makeListOfRepeatedBets(n - 1, bet)
    else List.empty
  }

  def placeBet(id: UUID, bet: Bet, player: Player, placedScores: Long)(
    implicit cacheOfPlayers: Cache[IO, UUID, Player],
    game: Game
  ): IO[String] = {
    for {
      number <- game.get
      _ <- cacheOfPlayers.update(
        id,
        Player(
          player.scores - placedScores,
          player.bets ++ List(bet),
          player.winningScores + bet.getResult(number),
          player.connection
        )
      )
      betType <- betTypeParser(bet.betType)
      numbers = bet.numbers.map(x => x.value).toString
      status <- IO(
        ToClient(
          BETS_OPEN,
          player.scores - placedScores,
          s"Bet - $betType, successfully placed on: $numbers"
        ).asJson.toString
      )
    } yield status
  }

  def processGameMessage(phaseUpdate: PhaseUpdate, id: UUID)(
    implicit cacheOfPlayers: Cache[IO, UUID, Player],
    game: Game
  ): IO[String] = {
    phaseUpdate.phase match {
      case BETS_OPEN =>
        generateToClientMessage(id, BETS_OPEN, "Please make your bets!")
      case BETS_CLOSED =>
        generateToClientMessage(id, BETS_CLOSED, "Calculating results...")
      case RESULT_ANNOUNCED => generateResultMessage(id)
    }
  }

  private def generateToClientMessage(id: UUID, phase: Phase, message: String)(
    implicit cacheOfPlayers: Cache[IO, UUID, Player],
    game: Game
  ): IO[String] = {
    for {
      player <- cacheOfPlayers.get(id)
      result = player match {
        case Some(value) =>
          ToClient(phase, value.scores, message).asJson.toString
        case None =>
          ErrorMessage("Error: Can't find a player").asJson.toString
      }
    } yield result
  }

  private def generateResultMessage(id: UUID)(
    implicit cacheOfPlayers: Cache[IO, UUID, Player],
    game: Game
  ): IO[String] = {
    for {
      player <- cacheOfPlayers.get(id)
      number <- game.get
      color = number.color match {
        case Red   => "red "
        case Black => "black "
        case _     => ""
      }
      result <- player match {
        case Some(p) =>
          for {
            message <- IO(
              ResultMessage(
                p.scores,
                p.winningScores,
                s"You won ${p.winningScores}. Drawn number was: $color${number.value}. ${p.scores + p.winningScores} left on your wallet."
              ).asJson.toString
            )
            _ <- cacheOfPlayers.update(
              id,
              Player(p.scores + p.winningScores, List.empty, 0, p.connection)
            )
          } yield message
        case None =>
          IO(ErrorMessage("Error: Can't find a player").asJson.toString)
      }
    } yield result
  }

  private def toEitherList(
    list: List[Either[String, models.Number]]
  ): Either[List[String], List[models.Number]] = {
    list.partition(_.isLeft) match {
      case (Nil, ints)  => Right(for (Right(i) <- ints) yield i)
      case (strings, _) => Left(for (Left(s) <- strings) yield s)
    }
  }

  private def betTypeParser(betType: String): IO[String] = {
    IO(betType match {
      case "Si" => "Single"
      case "Sp" => "Split"
      case "St" => "Street"
      case "Sq" => "Square"
      case "DS" => "Double street"
      case "Ba" => "Basket"
      case "FF" => "First four"
      case "Re" => "Red"
      case "Bl" => "Black"
      case "Ev" => "Even"
      case "Od" => "Odd"
      case "Sm" => "Small"
      case "Bi" => "Big"
      case "Do" => "Dozen"
      case "Ro" => "Row"
      case _    => "Incorrect bet type"
    })
  }

  private def webSocketApp(game: Game,
                           topic: Topic[IO, WebSocketFrame],
                           cacheOfPlayers: Cache[IO, UUID, Player]) = {
    implicit val impGame: Game = game
    implicit val impTopic: Topic[IO, WebSocketFrame] = topic
    implicit val impCacheOfPlayers: Cache[IO, UUID, Player] = cacheOfPlayers
    roulette
  }.orNotFound

  override def run(args: List[String]): IO[ExitCode] = {
    for {
      cacheOfPlayers <- Cache.of[IO, UUID, Player](3600.seconds, 100.seconds)
      initialMessage = PhaseUpdate(BETS_OPEN, "Game has started").asJson.toString
      topic <- Topic[IO, WebSocketFrame](WebSocketFrame.Text(initialMessage))
      game <- Game.of(cacheOfPlayers, topic)
      exitCode <- {
        BlazeServerBuilder[IO](ExecutionContext.global)
          .bindHttp(port = 9002, host = "localhost")
          .withHttpApp(webSocketApp(game, topic, cacheOfPlayers))
          .serve
          .compile
          .drain
          .as(ExitCode.Success)
      }
    } yield exitCode
  }
}
