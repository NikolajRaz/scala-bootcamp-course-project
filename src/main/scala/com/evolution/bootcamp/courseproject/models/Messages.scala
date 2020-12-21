package com.evolution.bootcamp.courseproject.models

import io.circe.{Decoder, Encoder}
import io.circe.generic.semiauto.{deriveDecoder, deriveEncoder}

object Messages {
  sealed trait ClientMessages
  final case class FromClient(place: Receive,
                              placedScores: Long,
                              betType: String,
                              placedNumbers: List[Int])
      extends ClientMessages
  sealed trait ServerMessages
  final case class ToClient(phase: Phase, scoresLeft: Long, message: String)
      extends ServerMessages
  final case class PhaseUpdate(phase: Phase, message: String)
      extends ServerMessages
  final case class ResultMessage(scoresLeft: Long,
                                 scoresWon: Long,
                                 message: String)
      extends ServerMessages
  final case class WarnMessage(message: String) extends ServerMessages
  final case class ErrorMessage(message: String) extends ServerMessages

  implicit val placeBetDecoder: Decoder[FromClient] = deriveDecoder
  implicit val placeBetEncoder: Encoder[FromClient] = deriveEncoder
  implicit val toClientDecoder: Decoder[ToClient] = deriveDecoder
  implicit val toClientEncoder: Encoder[ToClient] = deriveEncoder
  implicit val resultMessageDecoder: Decoder[ResultMessage] = deriveDecoder
  implicit val resultMessageEncoder: Encoder[ResultMessage] = deriveEncoder
  implicit val phaseDecoder: Decoder[Phase] = deriveDecoder
  implicit val phaseEncoder: Encoder[Phase] = deriveEncoder
  implicit val receiveDecoder: Decoder[Receive] = deriveDecoder
  implicit val receiveEncoder: Encoder[Receive] = deriveEncoder
  implicit val phaseUpdateDecoder: Decoder[PhaseUpdate] = deriveDecoder
  implicit val phaseUpdateEncoder: Encoder[PhaseUpdate] = deriveEncoder
  implicit val errorUpdateDecoder: Decoder[ErrorMessage] = deriveDecoder
  implicit val errorUpdateEncoder: Encoder[ErrorMessage] = deriveEncoder
  implicit val warnMessageDecoder: Decoder[WarnMessage] = deriveDecoder
  implicit val warnMessageEncoder: Encoder[WarnMessage] = deriveEncoder
}
