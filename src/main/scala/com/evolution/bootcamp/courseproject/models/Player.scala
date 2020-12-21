package com.evolution.bootcamp.courseproject.models

import cats.effect.IO
import fs2.concurrent.Queue
import org.http4s.websocket.WebSocketFrame

final case class Player(scores: Long, connection: Queue[IO, WebSocketFrame])
