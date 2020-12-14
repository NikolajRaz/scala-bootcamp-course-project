package com.evolution.bootcamp.courseproject

import cats.effect.{ExitCode, IO, IOApp}
import fs2.Pipe
import fs2.concurrent.Queue
import org.http4s._
import org.http4s.dsl.io._
import org.http4s.implicits._
import org.http4s.server.blaze.BlazeServerBuilder
import org.http4s.server.websocket.WebSocketBuilder
import org.http4s.websocket.WebSocketFrame

import scala.concurrent.duration._
import scala.concurrent.ExecutionContext

object Server extends IOApp {
  val defaultScores: Long = 100

  def newPlayer(queue: Queue[IO, WebSocketFrame],
                cache: Cache[IO, Int, Player]): HttpRoutes[IO] =
    HttpRoutes.of[IO] {
      case req @ GET -> Root / "start" =>
        val echoPipe: Pipe[IO, WebSocketFrame, WebSocketFrame] =
          _.collect {
            case WebSocketFrame.Text(message, other) =>
              WebSocketFrame.Text(message + other)
          }
        for {
          response <- WebSocketBuilder[IO].build(
            receive = queue.enqueue,
            send = queue.dequeue.through(echoPipe),
          )
        } yield response
    }

  private def webSocketApp(queue: Queue[IO, WebSocketFrame],
                           cacheOfPlayers: Cache[IO, Int, Player],
                           cacheOfBets: Cache[IO, Int, PlayerBet]) = {
    newPlayer(queue, cacheOfPlayers)
  }.orNotFound

  override def run(args: List[String]): IO[ExitCode] = {
    for {
      cacheOfPlayers <- Cache.of[IO, Int, Player](3600.seconds, 100.seconds)
      cacheOfBets <- Cache.of[IO, Int, PlayerBet](60.seconds, 5.seconds)
      queue <- Queue.unbounded[IO, WebSocketFrame]
      exitCode <- BlazeServerBuilder[IO](ExecutionContext.global)
        .bindHttp(port = 9002, host = "localhost")
        .withHttpApp(webSocketApp(queue, cacheOfPlayers, cacheOfBets))
        .serve
        .compile
        .drain
        .as(ExitCode.Success)
    } yield exitCode
  }
}
