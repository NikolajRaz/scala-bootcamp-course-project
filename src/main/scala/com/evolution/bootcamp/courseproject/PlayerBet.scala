package com.evolution.bootcamp.courseproject

final case class PlayerBet(playerId: Int, placedScores: Long, bet: Bet) {
  def getResult(number: Number): Either[String, Long] = {
    val result: Option[Long] = if (bet.numbers.contains(number)) {
      bet.betType match {
        case "Si" => Some(placedScores * 36)
        case "Sp" => Some(placedScores * 18)
        case "St" => Some(placedScores * 12)
        case "Sq" => Some(placedScores * 9)
        case "DS" => Some(placedScores * 6)
        case "Ba" => Some(placedScores * 6)
        case "FF" => Some(placedScores * 9)
        case "Re" => Some(placedScores * 2)
        case "Bl" => Some(placedScores * 2)
        case "Ev" => Some(placedScores * 2)
        case "Od" => Some(placedScores * 2)
        case "Sm" => Some(placedScores * 2)
        case "Bi" => Some(placedScores * 2)
        case "Do" => Some(placedScores * 3)
        case "Ro" => Some(placedScores * 3)
        case _    => None
      }
    } else Some(0)
    result match {
      case Some(v) => Right(v)
      case None    => Left("Error caused by bet type absence")
    }
  }
}
