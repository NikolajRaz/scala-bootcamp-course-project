package com.evolution.bootcamp.courseproject
import akka.{Done, NotUsed}
import akka.actor.ActorSystem
import akka.http.scaladsl.Http
import akka.http.scaladsl.model.StatusCodes
import akka.http.scaladsl.model.ws.{Message, TextMessage, WebSocketRequest}
import akka.stream.scaladsl.{Flow, Keep, Sink, Source}
import akka.stream.OverflowStrategy
import com.evolution.bootcamp.courseproject.models.{BETS_OPEN, PLACE_BET}
import com.evolution.bootcamp.courseproject.models.Messages.{
  ErrorMessage,
  FromClient,
  ResultMessage,
  ToClient,
  WarnMessage
}
import io.circe.parser.decode
import io.circe.syntax._
import io.circe.Decoder

import scala.concurrent.duration._

object Client {
  def main(args: Array[String]): Unit = {
    implicit val system = ActorSystem()
    import system.dispatcher

    val req = WebSocketRequest(uri = "ws://127.0.0.1:9002/roulette")
    val webSocketFlow = Http().webSocketClientFlow(req)

    val messageSink: Sink[Message, NotUsed] =
      Flow[Message]
        .map(message => {
          val decoded = message.asTextMessage.getStrictText match {
            case x if decodeJsonMessage[ToClient](x).isDefined =>
              val received =
                decodeJsonMessage[ToClient](x)
                  .getOrElse(ToClient(BETS_OPEN, 0, ""))
              received.message
            case x if decodeJsonMessage[ResultMessage](x).isDefined =>
              decodeJsonMessage[ResultMessage](x)
                .getOrElse(ResultMessage(0, 0, ""))
                .message
            case x if decodeJsonMessage[WarnMessage](x).isDefined =>
              decodeJsonMessage[WarnMessage](x)
                .getOrElse(WarnMessage(""))
                .message
            case x if decodeJsonMessage[ErrorMessage](x).isDefined =>
              decodeJsonMessage[ErrorMessage](x)
                .getOrElse(ErrorMessage(""))
                .message
            case x => x
          }
          println(s"Server: [${decoded}]")
        })
        .to(Sink.ignore)

    val helloSource =
      Source
        .actorRef[TextMessage.Strict](bufferSize = 10, OverflowStrategy.fail)

    val ((ws, upgradeResponse), closed) =
      helloSource
        .viaMat(webSocketFlow)(Keep.both)
        .toMat(messageSink)(Keep.both)
        .run()

    val connected = upgradeResponse.map { upgrade =>
      if (upgrade.response.status == StatusCodes.SwitchingProtocols) { Done } else {
        throw new RuntimeException
        (s"Connection failed: ${upgrade.response.status}")
      }
    }

    Thread.sleep(1.seconds.toMillis)
    println("Client: Placing bet on red values")
    ws ! TextMessage.Strict(
      FromClient(PLACE_BET, 10, "Re", List.empty).asJson.toString
    )
    println("Client: Placing bet on single 1")
    ws ! TextMessage.Strict(
      FromClient(PLACE_BET, 10, "Si", List(1)).asJson.toString
    )
    Thread.sleep(10.seconds.toMillis)
  }

  private def decodeJsonMessage[V](
    message: String
  )(implicit decoder: Decoder[V]): Option[V] = {
    decode[V](message) match {
      case Right(value) => Some(value)
      case Left(_)      => None
    }
  }
}
