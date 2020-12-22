package com.evolution.bootcamp.courseproject.models

import cats.effect.IO
import fs2.concurrent.Queue
import org.http4s.websocket.WebSocketFrame

final case class Player(scores: Long,
                        bets: List[Bet],
                        winningScores: Long,
                        queue: Queue[IO, WebSocketFrame])
