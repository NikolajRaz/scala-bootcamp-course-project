package com.evolution.bootcamp.courseproject

final case class Bet(betType: String,
                     numbers: List[Number],
                     placedScores: Long) {
  def getResult(number: Number): Result = {
    val result = if (numbers.contains(number)) {
      betType match {
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
    Result(result)
  }
}

object Bet {
  val streets = List(1, 4, 7, 10, 13, 16, 19, 22, 25, 28, 31, 34)

  def of(betType: String,
         numbers: List[Number],
         placedScores: Long): Either[String, Bet] = {
    val validNumbers: Option[List[Number]] = betType match {
      case "Si" => single(numbers)
      case "Sp" => split(numbers)
      case "St" => street(numbers)
      case "Sq" => square(numbers)
      case "DS" => doubleStreet(numbers)
      case "Ba" => basket(numbers)
      case "FF" => firstFour(numbers)
      case "Re" => red(numbers)
      case "Bl" => black(numbers)
      case "Ev" => even(numbers)
      case "Od" => odd(numbers)
      case "Sm" => small(numbers)
      case "Bi" => big(numbers)
      case "Do" => dozens(numbers)
      case "Ro" => rows(numbers)
      case _    => None
    }

    validNumbers match {
      case Some(value) if placedScores > 0 =>
        Right(Bet(betType, value, placedScores))
      case Some(_) => Left("Can't place bet that is lower than zero")
      case None    => Left("Wrong bet format")
    }
  }

  def generateNumbers(number: Int, n: Int): List[Number] =
    if (n > 0)
      List(Number(number)) ++ generateNumbers(number + 1, n - 1)
    else List.empty

  def single(numbers: List[Number]): Option[List[Number]] =
    if (numbers.length == 1) Some(numbers) else None

  def split(numbers: List[Number]): Option[List[Number]] =
    if (numbers.length == 2) {
      val values = numbers.map(_.value)
      val firstValue = values.maxOption
      val secondValue = values.minOption
      firstValue match {
        case Some(x) =>
          secondValue match {
            case Some(y) =>
              if (x - y == 1 || x - y == 3) Some(numbers) else None
            case None => None
          }
        case None => None
      }
    } else None

  def street(numbers: List[Number]): Option[List[Number]] =
    if (numbers.length == 1) {
      val values = numbers.map(_.value)
      val value = values.headOption
      value match {
        case Some(v) =>
          if (streets.contains(v))
            Some(generateNumbers(v, 3))
          else None
        case None => None
      }
    } else None

  def square(numbers: List[Number]): Option[List[Number]] =
    if (numbers.length == 4) {
      val values = numbers.map(_.value).sorted
      values match {
        case x :: xs =>
          xs match {
            case y :: ys if x + 1 == y =>
              ys match {
                case z :: zs if y + 2 == z =>
                  zs match {
                    case i :: _ if z + 1 == i => Some(numbers)
                    case _                    => None
                  }
                case _ => None
              }
            case _ => None
          }
        case _ => None
      }
    } else None

  def doubleStreet(numbers: List[Number]): Option[List[Number]] =
    if (numbers.length == 1) {
      val values = numbers.map(_.value)
      val value = values.headOption
      value match {
        case Some(v) =>
          if (streets.contains(v) && v != 34)
            Some(generateNumbers(v, 6))
          else None
        case None => None
      }
    } else None

  def basket(numbers: List[Number]): Option[List[Number]] =
    if (numbers.length == 1) {
      val values = numbers.map(_.value)
      val value = values.headOption
      value match {
        case Some(v) if v == 1 => Some(List(Number(0), Number(1), Number(2)))
        case Some(v) if v == 3 => Some(List(Number(0), Number(2), Number(3)))
        case _                 => None
      }
    } else None

  def firstFour(numbers: List[Number]): Option[List[Number]] =
    if (numbers.isEmpty) Some(List(Number(0), Number(1), Number(2), Number(3)))
    else None

  def red(numbers: List[Number]): Option[List[Number]] =
    if (numbers.isEmpty) Some(Number.redValues.map(x => Number(x))) else None

  def black(numbers: List[Number]): Option[List[Number]] =
    if (numbers.isEmpty) Some(Number.blackValues.map(x => Number(x))) else None

  def even(numbers: List[Number]): Option[List[Number]] =
    if (numbers.isEmpty) {
      val evenValues = 2 to 36 by 2
      Some(evenValues.toList.map(x => Number(x)))
    } else None

  def odd(numbers: List[Number]): Option[List[Number]] =
    if (numbers.isEmpty) {
      val oddValues = 1 to 36 by 2
      Some(oddValues.toList.map(x => Number(x)))
    } else None

  def small(numbers: List[Number]): Option[List[Number]] =
    if (numbers.isEmpty) {
      val values = 1 to 18
      Some(values.toList.map(x => Number(x)))
    } else None

  def big(numbers: List[Number]): Option[List[Number]] =
    if (numbers.isEmpty) {
      val values = 19 to 36
      Some(values.toList.map(x => Number(x)))
    } else None

  def dozens(numbers: List[Number]): Option[List[Number]] =
    if (numbers.length == 1) {
      val values = numbers.map(_.value)
      val value = values.headOption
      value match {
        case Some(v) =>
          if (v == 1 || v == 13 || v == 25)
            Some((v to v + 11).toList.map(x => Number(x)))
          else None
        case None => None
      }
    } else None

  def rows(numbers: List[Number]): Option[List[Number]] =
    if (numbers.length == 1) {
      val values = numbers.map(_.value)
      val value = values.headOption
      value match {
        case Some(v) =>
          if (v == 34 || v == 35 || v == 36)
            Some((v - 33 to v by 3).toList.map(x => Number(x)))
          else None
        case None => None
      }
    } else None
}
