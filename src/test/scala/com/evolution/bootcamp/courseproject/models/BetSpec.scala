package com.evolution.bootcamp.courseproject.models

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class BetSpec extends AnyFlatSpec with Matchers {
  val redValues = Number.redValues.map(x => Number(x))
  val blackValues = Number.blackValues.map(x => Number(x))
  val evenValues = (2 to 36 by 2).toList.map(x => Number(x))
  val oddValues = (1 to 36 by 2).toList.map(x => Number(x))
  val small = (1 to 18).toList.map(x => Number(x))
  val big = (19 to 36).toList.map(x => Number(x))
  val dozen = (1 to 12).toList.map(x => Number(x))
  val row = (2 to 35 by 3).toList.map(x => Number(x))

  it should "return correct results for single" in {
    Bet.of(SINGLE, List(Number(25)), 1) shouldEqual Right(
      Bet(SINGLE, List(Number(25)), 1)
    )
    Bet.of(SINGLE, List.empty, 1) shouldEqual Left("Wrong bet format")
    Bet.of(SINGLE, List(Number(25), Number(26)), 1) shouldEqual Left(
      "Wrong bet format"
    )
  }

  it should "return correct results for split" in {
    Bet.of(SPLIT, List(Number(25)), 1) shouldEqual Left("Wrong bet format")
    Bet.of(SPLIT, List(Number(25), Number(35)), 1) shouldEqual Left(
      "Wrong bet format"
    )
    Bet.of(SPLIT, List.empty, 1) shouldEqual Left("Wrong bet format")
    Bet.of(SPLIT, List(Number(25), Number(26)), 1) shouldEqual Right(
      Bet(SPLIT, List(Number(25), Number(26)), 1)
    )
    Bet.of(SPLIT, List(Number(25), Number(28)), 1) shouldEqual Right(
      Bet(SPLIT, List(Number(25), Number(28)), 1)
    )
  }

  it should "return correct results for street" in {
    Bet.of(STREET, List(Number(7)), 1) shouldEqual Right(
      Bet(STREET, List(Number(7), Number(8), Number(9)), 1)
    )
    Bet.of(STREET, List(Number(8)), 1) shouldEqual Left("Wrong bet format")
    Bet.of(STREET, List.empty, 1) shouldEqual Left("Wrong bet format")
    Bet.of(STREET, List(Number(25), Number(26)), 1) shouldEqual Left(
      "Wrong bet format"
    )
  }

  it should "return correct results for square" in {
    Bet.of(SQUARE, List(Number(30), Number(29), Number(32), Number(33)), 1) shouldEqual Right(
      Bet(SQUARE, List(Number(30), Number(29), Number(32), Number(33)), 1)
    )
    Bet.of(SQUARE, List(Number(25)), 1) shouldEqual Left("Wrong bet format")
    Bet.of(SQUARE, List(Number(25), Number(35), Number(29), Number(33)), 1) shouldEqual Left(
      "Wrong bet format"
    )
    Bet.of(SQUARE, List.empty, 1) shouldEqual Left("Wrong bet format")
  }

  it should "return correct results for double street" in {
    Bet.of(DOUBLE_STREET, List(Number(1)), 1) shouldEqual Right(
      Bet(
        DOUBLE_STREET,
        List(Number(1), Number(2), Number(3), Number(4), Number(5), Number(6)),
        1
      )
    )
    Bet.of(DOUBLE_STREET, List(Number(34)), 1) shouldEqual Left(
      "Wrong bet format"
    )
    Bet.of(DOUBLE_STREET, List(Number(8)), 1) shouldEqual Left(
      "Wrong bet format"
    )
    Bet.of(DOUBLE_STREET, List.empty, 1) shouldEqual Left("Wrong bet format")
    Bet.of(DOUBLE_STREET, List(Number(25), Number(26)), 1) shouldEqual Left(
      "Wrong bet format"
    )
  }

  it should "return correct results for basket" in {
    Bet.of(BASKET, List(Number(1)), 1) shouldEqual Right(
      Bet(BASKET, List(Number(0), Number(1), Number(2)), 1)
    )
    Bet.of(BASKET, List(Number(3)), 1) shouldEqual Right(
      Bet(BASKET, List(Number(0), Number(2), Number(3)), 1)
    )
    Bet.of(BASKET, List(Number(8)), 1) shouldEqual Left("Wrong bet format")
  }

  it should "return correct results for first four" in {
    Bet.of(FIRST_FOUR, List.empty, 1) shouldEqual Right(
      Bet(FIRST_FOUR, List(Number(0), Number(1), Number(2), Number(3)), 1)
    )
    Bet.of(FIRST_FOUR, List(Number(1)), 1) shouldEqual Left("Wrong bet format")
  }

  it should "return correct results for red" in {
    Bet.of(RED_NUMBERS, List.empty, 1) shouldEqual Right(
      Bet(
        RED_NUMBERS,
        List(
          Number(1),
          Number(3),
          Number(5),
          Number(7),
          Number(9),
          Number(12),
          Number(14),
          Number(16),
          Number(18),
          Number(19),
          Number(21),
          Number(23),
          Number(25),
          Number(27),
          Number(30),
          Number(32),
          Number(34),
          Number(36)
        ),
        1
      )
    )
    Bet.of(RED_NUMBERS, List(Number(1)), 1) shouldEqual Left("Wrong bet format")
  }

  it should "return correct results for black" in {
    Bet.of(BLACK_NUMBERS, List.empty, 1) shouldEqual Right(
      Bet(
        BLACK_NUMBERS,
        List(
          Number(2),
          Number(4),
          Number(6),
          Number(8),
          Number(10),
          Number(11),
          Number(13),
          Number(15),
          Number(17),
          Number(20),
          Number(22),
          Number(24),
          Number(26),
          Number(28),
          Number(29),
          Number(31),
          Number(33),
          Number(35)
        ),
        1
      )
    )
    Bet.of(BLACK_NUMBERS, List(Number(1)), 1) shouldEqual Left(
      "Wrong bet format"
    )
  }

  it should "return correct results for even" in {
    Bet.of(EVEN, List.empty, 1) shouldEqual Right(
      Bet(
        EVEN,
        List(
          Number(2),
          Number(4),
          Number(6),
          Number(8),
          Number(10),
          Number(12),
          Number(14),
          Number(16),
          Number(18),
          Number(20),
          Number(22),
          Number(24),
          Number(26),
          Number(28),
          Number(30),
          Number(32),
          Number(34),
          Number(36)
        ),
        1
      )
    )
    Bet.of(EVEN, List(Number(1)), 1) shouldEqual Left("Wrong bet format")
  }

  it should "return correct results for odd" in {
    Bet.of(ODD, List.empty, 1) shouldEqual Right(
      Bet(
        ODD,
        List(
          Number(1),
          Number(3),
          Number(5),
          Number(7),
          Number(9),
          Number(11),
          Number(13),
          Number(15),
          Number(17),
          Number(19),
          Number(21),
          Number(23),
          Number(25),
          Number(27),
          Number(29),
          Number(31),
          Number(33),
          Number(35)
        ),
        1
      )
    )
    Bet.of(ODD, List(Number(1)), 1) shouldEqual Left("Wrong bet format")
  }

  it should "return correct results for small" in {
    Bet.of(SMALL, List.empty, 1) shouldEqual Right(
      Bet(
        SMALL,
        List(
          Number(1),
          Number(2),
          Number(3),
          Number(4),
          Number(5),
          Number(6),
          Number(7),
          Number(8),
          Number(9),
          Number(10),
          Number(11),
          Number(12),
          Number(13),
          Number(14),
          Number(15),
          Number(16),
          Number(17),
          Number(18)
        ),
        1
      )
    )
    Bet.of(SMALL, List(Number(1)), 1) shouldEqual Left("Wrong bet format")
  }

  it should "return correct results for big" in {
    Bet.of(BIG, List.empty, 1) shouldEqual Right(
      Bet(
        BIG,
        List(
          Number(19),
          Number(20),
          Number(21),
          Number(22),
          Number(23),
          Number(24),
          Number(25),
          Number(26),
          Number(27),
          Number(28),
          Number(29),
          Number(30),
          Number(31),
          Number(32),
          Number(33),
          Number(34),
          Number(35),
          Number(36)
        ),
        1
      )
    )
    Bet.of(BIG, List(Number(1)), 1) shouldEqual Left("Wrong bet format")
  }

  it should "return correct results for dozens" in {
    Bet.of(DOZEN, List(Number(1)), 1) shouldEqual Right(
      Bet(
        DOZEN,
        List(
          Number(1),
          Number(2),
          Number(3),
          Number(4),
          Number(5),
          Number(6),
          Number(7),
          Number(8),
          Number(9),
          Number(10),
          Number(11),
          Number(12),
        ),
        1
      )
    )
    Bet.of(DOZEN, List(Number(13)), 1) shouldEqual Right(
      Bet(
        DOZEN,
        List(
          Number(13),
          Number(14),
          Number(15),
          Number(16),
          Number(17),
          Number(18),
          Number(19),
          Number(20),
          Number(21),
          Number(22),
          Number(23),
          Number(24)
        ),
        1
      )
    )
    Bet.of(DOZEN, List(Number(25)), 1) shouldEqual Right(
      Bet(
        DOZEN,
        List(
          Number(25),
          Number(26),
          Number(27),
          Number(28),
          Number(29),
          Number(30),
          Number(31),
          Number(32),
          Number(33),
          Number(34),
          Number(35),
          Number(36)
        ),
        1
      )
    )
    Bet.of(DOZEN, List(Number(2)), 1) shouldEqual Left("Wrong bet format")
  }

  it should "return correct results for rows" in {
    Bet.of(ROW, List(Number(34)), 1) shouldEqual Right(
      Bet(
        ROW,
        List(
          Number(1),
          Number(4),
          Number(7),
          Number(10),
          Number(13),
          Number(16),
          Number(19),
          Number(22),
          Number(25),
          Number(28),
          Number(31),
          Number(34),
        ),
        1
      )
    )
    Bet.of(ROW, List(Number(35)), 1) shouldEqual Right(
      Bet(
        ROW,
        List(
          Number(2),
          Number(5),
          Number(8),
          Number(11),
          Number(14),
          Number(17),
          Number(20),
          Number(23),
          Number(26),
          Number(29),
          Number(32),
          Number(35)
        ),
        1
      )
    )
    Bet.of(ROW, List(Number(36)), 1) shouldEqual Right(
      Bet(
        ROW,
        List(
          Number(3),
          Number(6),
          Number(9),
          Number(12),
          Number(15),
          Number(18),
          Number(21),
          Number(24),
          Number(27),
          Number(30),
          Number(33),
          Number(36)
        ),
        1
      )
    )
    Bet.of(ROW, List(Number(1)), 1) shouldEqual Left("Wrong bet format")
  }

  it should "return correct result for getResult function" in {
    Bet(SINGLE, List(Number(34)), 1).getResult(Number(34)) shouldEqual 36
    Bet(SPLIT, List(Number(33), Number(36)), 1)
      .getResult(Number(33)) shouldEqual 18
    Bet(STREET, List(Number(34), Number(35), Number(36)), 1)
      .getResult(Number(35)) shouldEqual 12
    Bet(SQUARE, List(Number(26), Number(29), Number(32), Number(35)), 1)
      .getResult(Number(32)) shouldEqual 9
    Bet(
      DOUBLE_STREET,
      List(
        Number(31),
        Number(32),
        Number(33),
        Number(34),
        Number(35),
        Number(36)
      ),
      1
    ).getResult(Number(34)) shouldEqual 6
    Bet(BASKET, List(Number(0), Number(2), Number(3)), 1)
      .getResult(Number(2)) shouldEqual 6
    Bet(FIRST_FOUR, List(Number(0), Number(1), Number(2), Number(3)), 1)
      .getResult(Number(2)) shouldEqual 9
    Bet(EVEN, evenValues, 1).getResult(Number(2)) shouldEqual 2
    Bet(ODD, oddValues, 1).getResult(Number(1)) shouldEqual 2
    Bet(RED_NUMBERS, redValues, 1).getResult(Number(34)) shouldEqual 2
    Bet(BLACK_NUMBERS, blackValues, 1).getResult(Number(35)) shouldEqual 2
    Bet(SMALL, small, 1).getResult(Number(10)) shouldEqual 2
    Bet(BIG, big, 1).getResult(Number(20)) shouldEqual 2
    Bet(DOZEN, dozen, 1).getResult(Number(12)) shouldEqual 3
    Bet(ROW, row, 1).getResult(Number(35)) shouldEqual 3
  }
}
