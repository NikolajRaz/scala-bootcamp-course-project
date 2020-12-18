package com.evolution.bootcamp.courseproject
import java.util.UUID

import cats.effect.{Clock, Concurrent, IO, Timer}
import cats.effect.concurrent.Ref
import cats.syntax.all._
import com.evolution.bootcamp.courseproject.Protocol.{ResultMessage, ToClient}
import io.circe.syntax._
import org.http4s.websocket.WebSocketFrame

import scala.concurrent.duration._

trait Game {
  def get: IO[Int]
  def getGamePhase: IO[Phase]
}

class RefGame(state: Ref[IO, (Long, Int, Phase)], expiresIn: FiniteDuration)
    extends Game {
  def get: IO[Int] = state.get.map { case (_, v, _)            => v }
  def getGamePhase: IO[Phase] = state.get.map { case (_, _, v) => v }
}

object Game {
  def of(
    cacheOfPlayers: Cache[IO, UUID, Player],
    cacheOfBets: Cache[IO, Int, PlayerBet],
    cacheOfResults: Cache[IO, UUID, Result]
  )(implicit T: Timer[IO], C: Concurrent[IO]): IO[Game] = {
    val expiresIn = 60.seconds
    val checkOnExpirationEvery = 1.seconds
    val secondPhaseDuration = 15.seconds
    val thirdPhaseDuration = 15.seconds
    val rand = scala.util.Random
    def expirationTimestamp(state: Ref[IO, (Long, Int, Phase)]): IO[Unit] = {
      for {
        _ <- T.sleep(checkOnExpirationEvery)
        curTime <- Clock[IO].realTime(MILLISECONDS)
        curState <- state.get
        allBets <- cacheOfBets.getAll
        allPlayerBets = allBets.map { case (_, (_, value)) => value }
        newState = curState match {
          //When round time is ended, new round starts
          case (expirationTime, _, _) if (curTime >= expirationTime) =>
            sendMessageToAllPlayers(cacheOfPlayers, "Time to make your bets!")
            (
              curTime + expiresIn.toMillis,
              0 + rand.nextInt((36 - 0) + 1),
              First
            )
          //Going to the third phase
          case (expirationTime, value, _)
              if curTime >= expirationTime - thirdPhaseDuration.toMillis =>
            val results =
              allPlayerBets.map(x => x.getResult(Number(value)))
            results.foreach(v => {
              for {
                _ <- cacheOfResults.get(v.playerId) map {
                  case Some(value) =>
                    for {
                      _ <- cacheOfResults.update(
                        v.playerId,
                        Result(v.playerId, value.result + v.result)
                      )
                    } yield ()
                  case None =>
                    for { _ <- cacheOfResults.put(v.playerId, v) } yield ()
                }
              } yield ()
            })
            for {
              summary <- cacheOfResults.getAll
              allResults = summary.map { case (_, (_, value)) => value }
              _ <- calculateResults(allResults, cacheOfPlayers)
            } yield ()
            (expirationTime, value, Third)
          // Going to the second phase
          case (expirationTime, value, _)
              if curTime >= expirationTime - secondPhaseDuration.toMillis - thirdPhaseDuration.toMillis =>
            sendMessageToAllPlayers(cacheOfPlayers, "Calculating result...")
            (expirationTime, value, Second)
          // Staying at current phase
          case _ => curState
        }
        _ <- state.set(newState)
      } yield ()
    }
    for {
      curTime <- Clock[IO].realTime(MILLISECONDS)
      newGame <- Ref.of[IO, (Long, Int, Phase)](
        curTime + expiresIn.toMillis,
        0 + rand.nextInt((36 - 0) + 1),
        First
      )
      _ <- C.start(expirationTimestamp(newGame).foreverM.void)
    } yield new RefGame(newGame, expiresIn)
  }

  private def calculateResults(
    results: Iterable[Result],
    cacheOfPlayers: Cache[IO, UUID, Player]
  ): IO[Unit] = {
    IO(results.map { v =>
      {
        for {
          player <- cacheOfPlayers.get(v.playerId)
          _ = player match {
            case Some(value) =>
              val resultMessage = ResultMessage(
                value.scores,
                v.result,
                s"Current balance is - ${value.scores + v.result}, last win - ${v.result}"
              )
              for {
                _ <- cacheOfPlayers.update(
                  value.id,
                  Player(value.id, value.scores + v.result, value.connection)
                )
                _ <- value.connection.enqueue1(
                  WebSocketFrame.Text(resultMessage.asJson.toString)
                )
              } yield ()

          }
        } yield ()
      }
    })
  }

  private def sendMessageToAllPlayers(cache: Cache[IO, UUID, Player],
                                      message: String): IO[Unit] = {
    for {
      mapWithPlayers <- cache.getAll
      allPlayers = mapWithPlayers.map { case (_, (_, value)) => value }
      _ = allPlayers.foreach(v => {
        for {
          _ <- v.connection.enqueue1(
            WebSocketFrame.Text(ToClient(v.scores, message).asJson.toString)
          )
        } yield ()
      })
    } yield ()
  }
}
