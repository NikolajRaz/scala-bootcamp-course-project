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
              val string: String = fromClient match {
                case Right(value) =>
                  inspectPlayerMessage(
                    value,
                    queue,
                    cacheOfPlayers,
                    cacheOfBets,
                    countOfPlayers,
                    countOfBets
                  )
                case Left(error) => error.toString
              }
              WebSocketFrame.Text(string)
            }
          }
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
                                   countOfBets: Ref[IO, Int]): String = {

    if (fromClient.id == 0) {
      countOfPlayers.modify(
        x => (x + 1, IO(cacheOfPlayers.put(x, Player(x, defaultScores))))
      )
      "New player was created, please make your bets"
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
              val playerBet =
                PlayerBet(fromClient.id, fromClient.placedScores, value)
              val status = for {
                player <- cacheOfPlayers.get(fromClient.id)
                placedScores = fromClient.placedScores
                status <- player match {
                  case Some(value) if value.scores >= placedScores =>
                    cacheOfPlayers.update(
                      fromClient.id,
                      Player(fromClient.id, value.scores - placedScores)
                    )
                    countOfBets.modify(
                      x => (x + 1, IO(cacheOfBets.put(x, playerBet)))
                    )
                    IO("Bet was successfully placed")
                  case Some(value) =>
                    IO(s"Not enough scores on your wallet - $value")
                  case None => IO("No such user")
                }
              } yield status
              //проблема в том что статус находится в IO
              "Operation is done: "
            case Left(error) => error
          }
        case _ => "Incorrect numbers format"
      }
      message
    }
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
