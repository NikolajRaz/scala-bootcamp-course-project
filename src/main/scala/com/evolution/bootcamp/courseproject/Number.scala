package com.evolution.bootcamp.courseproject

import cats.syntax.either._
import org.scalactic.ErrorMessage

final case class Number(value: Int) {
  def color: Color = value match {
    case 0                                   => Green
    case v if Number.blackValues.contains(v) => Black
    case v if Number.redValues.contains(v)   => Red
  }
}

object Number {
  val redValues =
    List(1, 3, 5, 7, 9, 12, 14, 16, 18, 19, 21, 23, 25, 27, 30, 32, 34, 36)

  val blackValues =
    List(2, 4, 6, 8, 10, 11, 13, 15, 17, 20, 22, 24, 26, 28, 29, 31, 33, 35)

  def of(value: Int): Either[ErrorMessage, Number] = {
    if (value < 0 || value > 36) s"Incorrect value of a number - $value".asLeft
    else Number(value).asRight
  }

}
