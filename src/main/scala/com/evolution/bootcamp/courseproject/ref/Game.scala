package com.evolution.bootcamp.courseproject.ref

import java.util.UUID

import cats.effect.{Clock, Concurrent, IO, Timer}
import cats.effect.concurrent.Ref
import cats.syntax.all._
import com.evolution.bootcamp.courseproject.models.{
  BETS_CLOSED,
  BETS_OPEN,
  Number,
  Phase,
  Player,
  RESULT_ANNOUNCED
}
import com.evolution.bootcamp.courseproject.models.Messages.PhaseUpdate
import fs2.concurrent.Topic
import io.circe.syntax._
import org.http4s.websocket.WebSocketFrame

import scala.concurrent.duration._

trait Game {
  def get: IO[Number]
  def getGamePhase: IO[Phase]
}

class RefGame(state: Ref[IO, (Long, Number, Phase)], expiresIn: FiniteDuration)
    extends Game {
  def get: IO[Number] = state.get.map { case (_, v, _)         => v }
  def getGamePhase: IO[Phase] = state.get.map { case (_, _, v) => v }
}

object Game {
  def of(
    cacheOfPlayers: Cache[IO, UUID, Player],
    t: Topic[IO, WebSocketFrame]
  )(implicit T: Timer[IO], C: Concurrent[IO]): IO[Game] = {
    implicit val topic: Topic[IO, WebSocketFrame] = t
    val expiresIn = 30.seconds
    val checkOnExpirationEvery = 10.seconds
    val secondPhaseDuration = 10.seconds
    val thirdPhaseDuration = 10.seconds
    val rand = scala.util.Random

    def expirationTimestamp(state: Ref[IO, (Long, Number, Phase)]): IO[Unit] = {
      for {
        _ <- T.sleep(checkOnExpirationEvery)
        curTime <- Clock[IO].realTime(MILLISECONDS)
        curState <- state.get
        newState = curState match {
          //When round time is ended, new round starts
          case (expirationTime, _, _) if (curTime >= expirationTime) =>
            (
              curTime + expiresIn.toMillis,
              Number(0 + rand.nextInt((36 - 0) + 1)),
              BETS_OPEN
            )
          //Going to the third phase
          case (expirationTime, value, _)
              if curTime >= expirationTime - thirdPhaseDuration.toMillis =>
            (expirationTime, value, RESULT_ANNOUNCED)
          //Going to the second phase
          case (expirationTime, value, _)
              if curTime >= expirationTime - secondPhaseDuration.toMillis - thirdPhaseDuration.toMillis =>
            (expirationTime, value, BETS_CLOSED)
          //Not changing game phase
          case _ => curState
        }
        _ <- newState match {
          case (_, _, v) =>
            sendToAll(PhaseUpdate(v, "").asJson.toString)
        }
        _ <- state.set(newState)
      } yield ()
    }
    for {
      curTime <- Clock[IO].realTime(MILLISECONDS)
      newGame <- Ref.of[IO, (Long, Number, Phase)](
        curTime + expiresIn.toMillis,
        Number(0 + rand.nextInt((36 - 0) + 1)),
        BETS_OPEN
      )
      _ <- C.start(expirationTimestamp(newGame).foreverM.void)
    } yield new RefGame(newGame, expiresIn)
  }

  def sendToAll(message: String)(
    implicit topic: Topic[IO, WebSocketFrame]
  ): IO[Unit] = topic.publish1(WebSocketFrame.Text(message))
}
