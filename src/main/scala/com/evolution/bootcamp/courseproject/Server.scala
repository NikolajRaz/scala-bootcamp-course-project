package com.evolution.bootcamp.courseproject

import cats.effect.{ExitCode, IO, IOApp}
import cats.effect.concurrent.Ref
import com.evolution.bootcamp.courseproject.Protocol.FromClient
import fs2.concurrent.Queue
import fs2.Pipe
import io.circe._
import io.circe.parser._
import org.http4s._
import org.http4s.dsl.io._
import org.http4s.implicits._
import org.http4s.server.blaze.BlazeServerBuilder
import org.http4s.server.websocket.WebSocketBuilder
import org.http4s.websocket.WebSocketFrame

import scala.concurrent.duration._
import scala.concurrent.ExecutionContext

//websocat "ws://127.0.0.1:9002/roulette"
//{"id": "1", "placedScores": "10", "betType": "Si", "placedNumbers": [1]}

object Protocol {
  final case class FromClient(id: Int,
                              placedScores: Long,
                              betType: String,
                              placedNumbers: List[Int])
  final case class ToClient(message: String)

  implicit val fromClientDecoder: Decoder[FromClient] =
    Decoder.forProduct4("id", "placedScores", "betType", "placedNumbers")(
      FromClient.apply
    )
  implicit val fromClientEncoder: Encoder[FromClient] =
    Encoder.forProduct4("id", "placedScores", "betType", "placedNumbers")(
      v => (v.id, v.placedScores, v.betType, v.placedNumbers)
    )
}

object Server extends IOApp {
  val defaultScores: Long = 100

  def roulette(cacheOfPlayers: Cache[IO, Int, Player],
               cacheOfBets: Cache[IO, Int, PlayerBet],
               countOfPlayers: Ref[IO, Int],
               countOfBets: Ref[IO, Int]): HttpRoutes[IO] =
    HttpRoutes.of[IO] {
      case req @ GET -> Root / "roulette" =>
        def echoPipe(
          queue: Queue[IO, WebSocketFrame]
        ): Pipe[IO, WebSocketFrame, WebSocketFrame] = {
          _.collect {
            case WebSocketFrame.Text(message, _) => {
              val fromClient: Either[Error, FromClient] =
                decode[FromClient](message)
              val string: IO[String] = fromClient match {
                case Right(value) =>
                  inspectPlayerMessage(
                    value,
                    queue,
                    cacheOfPlayers,
                    cacheOfBets,
                    countOfPlayers,
                    countOfBets
                  )
                case Left(error) => IO(error.toString)
              }
              for {
                message <- string
                response = WebSocketFrame.Text(message)
              } yield response
            }
          }.evalMap(text => text)
        }

        for {
          queue <- Queue.unbounded[IO, WebSocketFrame]
          response <- WebSocketBuilder[IO].build(
            receive = queue.enqueue,
            send = queue.dequeue.through(echoPipe(queue)),
          )
        } yield response
    }

  private def inspectPlayerMessage(fromClient: FromClient,
                                   queue: Queue[IO, WebSocketFrame],
                                   cacheOfPlayers: Cache[IO, Int, Player],
                                   cacheOfBets: Cache[IO, Int, PlayerBet],
                                   countOfPlayers: Ref[IO, Int],
                                   countOfBets: Ref[IO, Int]): IO[String] = {

    if (fromClient.id == 0) {
      val res = for {
        count <- countOfPlayers.get
        _ <- cacheOfPlayers.put(count, Player(count, defaultScores))
        _ <- countOfPlayers.update(x => x + 1)
        result <- IO("New player was created, please make your bets")
      } yield result
      res
    } else {
      val placedNumbers =
        toEitherList(
          fromClient.placedNumbers
            .map(x => Number.of(x))
        )

      val message = placedNumbers match {
        case Right(x) =>
          val bet =
            Bet.of(fromClient.betType, x)
          bet match {
            case Right(value) =>
              checkBalance(
                value,
                fromClient,
                queue,
                cacheOfPlayers,
                cacheOfBets,
                countOfPlayers,
                countOfBets
              )
            case Left(error) => IO(error)
          }
        case _ => IO("Incorrect numbers format")
      }
      message
    }
  }

  private def checkBalance(bet: Bet,
                           fromClient: FromClient,
                           queue: Queue[IO, WebSocketFrame],
                           cacheOfPlayers: Cache[IO, Int, Player],
                           cacheOfBets: Cache[IO, Int, PlayerBet],
                           countOfPlayers: Ref[IO, Int],
                           countOfBets: Ref[IO, Int]): IO[String] = {
    val playerBet =
      PlayerBet(fromClient.id, fromClient.placedScores, bet)
    for {
      player <- cacheOfPlayers.get(fromClient.id)
      placedScores = fromClient.placedScores
      count <- countOfBets.get
      status <- player match {
        case Some(value) if value.scores >= placedScores =>
          for {
            _ <- cacheOfPlayers.update(
              fromClient.id,
              Player(fromClient.id, value.scores - placedScores)
            )
            _ <- cacheOfBets.put(count, playerBet)
            _ <- countOfBets.update(x => x + 1)
            result <- IO(s"Bet was successfully placed $count")
          } yield result
        case Some(value) => IO(s"Not enough scores on wallet - ${value.scores}")
        case None        => IO(s"There is no user with id - ${fromClient.id}")
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

  private def webSocketApp(cacheOfPlayers: Cache[IO, Int, Player],
                           cacheOfBets: Cache[IO, Int, PlayerBet],
                           countOfPlayers: Ref[IO, Int],
                           countOfBets: Ref[IO, Int]) = {
    roulette(cacheOfPlayers, cacheOfBets, countOfPlayers, countOfBets)
  }.orNotFound

  override def run(args: List[String]): IO[ExitCode] = {
    for {
      cacheOfPlayers <- Cache.of[IO, Int, Player](3600.seconds, 100.seconds)
      cacheOfBets <- Cache.of[IO, Int, PlayerBet](60.seconds, 5.seconds)
      countOfPlayers <- Ref.of[IO, Int](1)
      countOfBets <- Ref.of[IO, Int](0)
      q <- Queue.unbounded[IO, WebSocketFrame]
      exitCode <- {
        BlazeServerBuilder[IO](ExecutionContext.global)
          .bindHttp(port = 9002, host = "localhost")
          .withHttpApp(
            webSocketApp(
              cacheOfPlayers,
              cacheOfBets,
              countOfPlayers,
              countOfBets
            )
          )
          .serve
          .compile
          .drain
          .as(ExitCode.Success)
      }
    } yield exitCode
  }
}
