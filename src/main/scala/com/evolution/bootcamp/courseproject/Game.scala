package com.evolution.bootcamp.courseproject
import cats.effect.{Clock, Concurrent, IO, Timer}
import cats.effect.concurrent.Ref
import cats.implicits._

import scala.concurrent.duration._

trait Game {
  def get: IO[Int]
  def getGamePhase: IO[Int]
}

class RefGame(state: Ref[IO, (Long, Int, Int)], expiresIn: FiniteDuration)
    extends Game {
  def get: IO[Int] = state.get.map { case (_, v, _)          => v }
  def getGamePhase: IO[Int] = state.get.map { case (_, _, v) => v }
}

object Game {
  def of(implicit T: Timer[IO], C: Concurrent[IO]): IO[Game] = {
    val expiresIn = 60.seconds
    val checkOnExpirationEvery = 1.seconds
    val secondPhaseDuration = 15.seconds
    val thirdPhaseDuration = 15.seconds
    val rand = scala.util.Random
    def expirationTimestamp(state: Ref[IO, (Long, Int, Int)]): IO[Unit] = {
      for {
        _ <- T.sleep(checkOnExpirationEvery)
        curTime <- Clock[IO].realTime(MILLISECONDS)
        curState <- state.get
        newState = curState match {
          case (expirationTime, _, _) if (curTime >= expirationTime) =>
            (curTime + expiresIn.toMillis, 0 + rand.nextInt((36 - 0) + 1), 1)
          case (expirationTime, value, _)
              if curTime >= expirationTime - thirdPhaseDuration.toMillis =>
            (expirationTime, value, 3)
          case (expirationTime, value, _)
              if curTime >= expirationTime - secondPhaseDuration.toMillis - thirdPhaseDuration.toMillis =>
            (expirationTime, value, 2)
          case _ => curState
        }
        _ <- state.set(newState)
      } yield ()
    }
    for {
      curTime <- Clock[IO].realTime(MILLISECONDS)
      newGame <- Ref.of[IO, (Long, Int, Int)](
        curTime + expiresIn.toMillis,
        0 + rand.nextInt((36 - 0) + 1),
        1
      )
      _ <- C.start(expirationTimestamp(newGame).foreverM.void)
    } yield new RefGame(newGame, expiresIn)
  }
}
