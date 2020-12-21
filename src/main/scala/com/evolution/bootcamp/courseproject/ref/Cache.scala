package com.evolution.bootcamp.courseproject.ref

import cats.implicits._
import cats.Monad
import cats.effect.{Clock, Concurrent, Timer}
import cats.effect.concurrent.Ref

import scala.concurrent.duration._

trait Cache[F[_], K, V] {
  def get(key: K): F[Option[V]]

  def put(key: K, value: V): F[Unit]

  def update(key: K, value: V): F[Unit]

  def remove(key: K): F[Unit]
}

class RefCache[F[_]: Clock: Monad, K, V](state: Ref[F, Map[K, (Long, V)]],
                                         expiresIn: FiniteDuration)
    extends Cache[F, K, V] {

  def get(key: K): F[Option[V]] =
    state.get.map(v => v.get(key).map { case (_, v) => v })

  def put(key: K, value: V): F[Unit] =
    Clock[F]
      .realTime(MILLISECONDS)
      .flatMap(
        curTime =>
          state.update(
            curState =>
              curState ++ Map(key -> (curTime + expiresIn.toMillis, value))
        )
      )

  def update(key: K, value: V): F[Unit] =
    Clock[F]
      .realTime(MILLISECONDS)
      .flatMap(
        curTime =>
          state
            .update(v => v.updated(key, (curTime + expiresIn.toMillis, value)))
      )

  def remove(key: K): F[Unit] = state.update(v => v.removed(key))
}

object Cache {
  def of[F[_]: Clock, K, V](
    expiresIn: FiniteDuration,
    checkOnExpirationsEvery: FiniteDuration
  )(implicit T: Timer[F], C: Concurrent[F]): F[Cache[F, K, V]] = {
    def expirationTimestamp(state: Ref[F, Map[K, (Long, V)]]): F[Unit] = {
      for {
        _ <- T.sleep(checkOnExpirationsEvery)
        curTime <- Clock[F].realTime(MILLISECONDS)
        curState <- state.get
        newState = curState.filter {
          case (_, (expirationTime, _)) =>
            curTime - expirationTime < expiresIn.toMillis
        }
        _ <- state.set(newState)
      } yield ()
    }

    for {
      newCache <- Ref.of[F, Map[K, (Long, V)]](Map.empty)
      _ <- C.start(expirationTimestamp(newCache).foreverM.void)
    } yield new RefCache[F, K, V](newCache, expiresIn)
  }

}
