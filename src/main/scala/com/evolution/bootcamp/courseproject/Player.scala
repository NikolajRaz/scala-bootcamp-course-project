package com.evolution.bootcamp.courseproject

import java.util.UUID

import cats.effect.IO
import fs2.concurrent.Queue
import org.http4s.websocket.WebSocketFrame

final case class Player(id: UUID,
                        scores: Long,
                        connection: Queue[IO, WebSocketFrame])
