package com.evolution.bootcamp.courseproject

import java.util.UUID

import cats.effect.{ExitCode, IO, IOApp}
import com.evolution.bootcamp.courseproject.Protocol.{
  ErrorMessage,
  FromClient,
  PhaseUpdate,
  ResultMessage,
  ToClient,
  WarnMessage
}
import fs2.concurrent.Queue
import fs2.{Pipe, Stream}
import io.circe._
import io.circe.generic.semiauto._
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
//{"placedScores": "10", "betType": "Re", "placedNumbers": []}

object Protocol {
  final case class FromClient(placedScores: Long,
                              betType: String,
                              placedNumbers: List[Int])
  final case class ToClient(scoresLeft: Long, message: String)
  final case class PhaseUpdate(phase: Phase, message: String)
  final case class ResultMessage(scoresLeft: Long,
                                 scoresWon: Long,
                                 message: String)
  final case class WarnMessage(message: String)
  final case class ErrorMessage(message: String)

  implicit val fromClientDecoder: Decoder[FromClient] = deriveDecoder
  implicit val fromClientEncoder: Encoder[FromClient] = deriveEncoder
  implicit val toClientDecoder: Decoder[ToClient] = deriveDecoder
  implicit val toClientEncoder: Encoder[ToClient] = deriveEncoder
  implicit val resultMessageDecoder: Decoder[ResultMessage] = deriveDecoder
  implicit val resultMessageEncoder: Encoder[ResultMessage] = deriveEncoder
  implicit val phaseDecoder: Decoder[Phase] = deriveDecoder
  implicit val phaseEncoder: Encoder[Phase] = deriveEncoder
  implicit val phaseUpdateDecoder: Decoder[PhaseUpdate] = deriveDecoder
  implicit val phaseUpdateEncoder: Encoder[PhaseUpdate] = deriveEncoder
  implicit val errorUpdateDecoder: Decoder[ErrorMessage] = deriveDecoder
  implicit val errorUpdateEncoder: Encoder[ErrorMessage] = deriveEncoder
  implicit val warnMessageDecoder: Decoder[WarnMessage] = deriveDecoder
  implicit val warnMessageEncoder: Encoder[WarnMessage] = deriveEncoder
}

object Server extends IOApp {
  val defaultScores: Long = 100

  def roulette(game: Game,
               generalQueue: Queue[IO, WebSocketFrame],
               cacheOfPlayers: Cache[IO, UUID, Player],
               cacheOfResults: Cache[IO, UUID, Result]): HttpRoutes[IO] =
    HttpRoutes.of[IO] {
      case req @ GET -> Root / "roulette" =>
        def echoPipe(queue: Queue[IO, WebSocketFrame],
                     id: UUID): Pipe[IO, WebSocketFrame, WebSocketFrame] = {
          _.collect {
            case WebSocketFrame.Text(message, _) => {
              val string = decode[FromClient](message) match {
                case Right(value) =>
                  inspectPlayerMessage(
                    id,
                    game,
                    value,
                    cacheOfPlayers,
                    cacheOfResults
                  )

                case Left(error) =>
                  IO(ErrorMessage(error.toString).asJson.toString)
              }
              for {
                message <- string
                response = WebSocketFrame.Text(message)
              } yield response
            }
          }.evalMap(text => text)
        }

        def generalPipe(queue: Queue[IO, WebSocketFrame],
                        id: UUID,
        ): Pipe[IO, WebSocketFrame, WebSocketFrame] = {
          _.collect {
            case WebSocketFrame.Text(message, _) =>
              val string = decode[PhaseUpdate](message) match {
                case Right(value) =>
                  value.phase match {
                    case First =>
                      for {
                        player <- cacheOfPlayers.get(id)
                        result = player match {
                          case Some(value) =>
                            ToClient(value.scores, "Please make your bets!").asJson.toString
                          case None =>
                            ErrorMessage("Error: Can't find a player").asJson.toString
                        }
                      } yield result
                    case Second =>
                      IO(WarnMessage("Calculating results...").asJson.toString)
                    case Third =>
                      for {
                        player <- cacheOfPlayers.get(id)
                        playerResult <- cacheOfResults.get(id)
                        number <- game.get
                        result = player match {
                          case Some(p) =>
                            playerResult match {
                              case Some(r) =>
                                ResultMessage(
                                  p.scores,
                                  r.winningScores,
                                  s"You won ${r.winningScores}. Drawn number was: $number. ${p.scores} left on your wallet."
                                ).asJson.toString
                              case None =>
                                WarnMessage(
                                  "Warn: You did not made any bets in this round"
                                ).asJson.toString
                            }
                          case None =>
                            ErrorMessage("Error: Can't find a player").asJson.toString
                        }
                      } yield result
                  }
                case Left(error) =>
                  IO(ErrorMessage(error.toString).asJson.toString)
              }
              for {
                message <- string
                response = WebSocketFrame.Text(message)
              } yield response

          }.evalMap(text => text)
        }

        for {
          queue <- Queue.unbounded[IO, WebSocketFrame]
          id = UUID.randomUUID()
          _ <- cacheOfPlayers.put(id, Player(id, defaultScores, queue))
          combinedStream = Stream(
            queue.dequeue.through(echoPipe(queue, id)),
            generalQueue.dequeue.through(generalPipe(queue, id))
          ).parJoinUnbounded
          response <- WebSocketBuilder[IO]
            .build(receive = queue.enqueue, send = combinedStream)
        } yield response
    }

  private def inspectPlayerMessage(
    id: UUID,
    game: Game,
    fromClient: FromClient,
    cacheOfPlayers: Cache[IO, UUID, Player],
    cacheOfResults: Cache[IO, UUID, Result]
  ): IO[String] = {
    for {
      gamePhase <- game.getGamePhase
      result <- gamePhase match {
        case First =>
          val placedNumbers =
            toEitherList(
              fromClient.placedNumbers
                .map(x => Number.of(x))
            )

          val message = placedNumbers match {
            case Right(x) =>
              val bet =
                Bet.of(fromClient.betType, x, fromClient.placedScores)
              bet match {
                case Right(value) =>
                  checkBalance(
                    id,
                    value,
                    game,
                    fromClient,
                    cacheOfPlayers,
                    cacheOfResults
                  )
                case Left(error) => IO(ErrorMessage(error).asJson.toString)
              }
            case _ =>
              IO(
                ErrorMessage("Error: Incorrect numbers format").asJson.toString
              )
          }
          message
        case _ =>
          IO(
            WarnMessage("Warn: You can't place bets in this game phase!").asJson.toString
          )
      }
    } yield result

  }

  private def checkBalance(
    id: UUID,
    bet: Bet,
    game: Game,
    fromClient: FromClient,
    cacheOfPlayers: Cache[IO, UUID, Player],
    cacheOfResults: Cache[IO, UUID, Result]
  ): IO[String] = {
    for {
      player <- cacheOfPlayers.get(id)
      placedScores = fromClient.placedScores
      status <- player match {
        case Some(p) if p.scores >= placedScores =>
          for {
            playerBets <- cacheOfResults.get(id)
            _ <- cacheOfPlayers.update(
              id,
              Player(id, p.scores - placedScores, p.connection)
            )
            number <- game.get
            currentBet = bet.getResult(Number(number))
            status <- playerBets match {
              case Some(s) =>
                for {
                  _ <- cacheOfResults.update(
                    id,
                    Result(s.winningScores + currentBet.winningScores)
                  )
                  json = ToClient(
                    p.scores - placedScores,
                    "Bet successfully placed"
                  ).asJson.toString
                } yield json
              case None =>
                for {
                  _ <- cacheOfResults.put(id, currentBet)
                  json = ToClient(
                    p.scores - placedScores,
                    "Bet successfully placed"
                  ).asJson.toString
                } yield json
            }
          } yield status
        case Some(p) =>
          IO(
            ErrorMessage(s"Not enough scores on your wallet - ${p.scores}").asJson.toString
          )
        case None => IO(ErrorMessage("Can't find user").asJson.toString)
      }
    } yield status
  }

  private def toEitherList(
    list: List[Either[String, Number]]
  ): Either[List[String], List[Number]] = {
    list.partition(_.isLeft) match {
      case (Nil, ints)  => Right(for (Right(i) <- ints) yield i)
      case (strings, _) => Left(for (Left(s) <- strings) yield s)
    }
  }

  private def webSocketApp(game: Game,
                           queue: Queue[IO, WebSocketFrame],
                           cacheOfPlayers: Cache[IO, UUID, Player],
                           cacheOfResults: Cache[IO, UUID, Result]) = {
    roulette(game, queue, cacheOfPlayers, cacheOfResults)
  }.orNotFound

  override def run(args: List[String]): IO[ExitCode] = {
    for {
      cacheOfPlayers <- Cache.of[IO, UUID, Player](3600.seconds, 100.seconds)
      cacheOfResults <- Cache.of[IO, UUID, Result](20.seconds, 5.seconds)
      queue <- Queue.unbounded[IO, WebSocketFrame]
      game <- Game.of(cacheOfPlayers, cacheOfResults, queue)
      exitCode <- {
        BlazeServerBuilder[IO](ExecutionContext.global)
          .bindHttp(port = 9002, host = "localhost")
          .withHttpApp(
            webSocketApp(game, queue, cacheOfPlayers, cacheOfResults)
          )
          .serve
          .compile
          .drain
          .as(ExitCode.Success)
      }
    } yield exitCode
  }
}
