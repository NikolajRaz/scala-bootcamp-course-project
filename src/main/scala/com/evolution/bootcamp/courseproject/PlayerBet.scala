package com.evolution.bootcamp.courseproject

import java.util.UUID

final case class PlayerBet(playerId: UUID, placedScores: Long, bet: Bet) {
  def getResult(number: Number): Result = {
    val result = if (bet.numbers.contains(number)) {
      bet.betType match {
        case "Si" => placedScores * 36
        case "Sp" => placedScores * 18
        case "St" => placedScores * 12
        case "Sq" => placedScores * 9
        case "DS" => placedScores * 6
        case "Ba" => placedScores * 6
        case "FF" => placedScores * 9
        case "Re" => placedScores * 2
        case "Bl" => placedScores * 2
        case "Ev" => placedScores * 2
        case "Od" => placedScores * 2
        case "Sm" => placedScores * 2
        case "Bi" => placedScores * 2
        case "Do" => placedScores * 3
        case "Ro" => placedScores * 3
      }
    } else 0
    Result(playerId, result)
  }
}
