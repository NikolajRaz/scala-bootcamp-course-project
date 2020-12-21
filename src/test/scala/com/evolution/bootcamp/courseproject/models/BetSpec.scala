package com.evolution.bootcamp.courseproject.models

import com.evolution.bootcamp.courseproject.models
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class BetSpec extends AnyFlatSpec with Matchers {
  val redValues = Number.redValues.map(x => Number(x))
  val blackValues = Number.blackValues.map(x => Number(x))
  val evenValues = (2 to 36 by 2).toList.map(x => Number(x))
  val oddValues = (1 to 36 by 2).toList.map(x => Number(x))
  val dozen = (1 to 12).toList.map(x => Number(x))
  val row = (2 to 35 by 3).toList.map(x => Number(x))

  it should "return correct results for single" in {
    Bet.of("Si", List(Number(25)), 1) shouldEqual Right(
      Bet("Si", List(Number(25)), 1)
    )
    Bet.of("Si", List.empty, 1) shouldEqual Left("Wrong bet format")
    Bet.of("Si", List(Number(25), Number(26)), 1) shouldEqual Left(
      "Wrong bet format"
    )
  }

  it should "return correct results for split" in {
    Bet.of("Sp", List(Number(25)), 1) shouldEqual Left("Wrong bet format")
    Bet.of("Sp", List(Number(25), Number(35)), 1) shouldEqual Left(
      "Wrong bet format"
    )
    Bet.of("Sp", List.empty, 1) shouldEqual Left("Wrong bet format")
    Bet.of("Sp", List(Number(25), Number(26)), 1) shouldEqual Right(
      Bet("Sp", List(Number(25), Number(26)), 1)
    )
    Bet.of("Sp", List(Number(25), Number(28)), 1) shouldEqual Right(
      Bet("Sp", List(Number(25), Number(28)), 1)
    )
  }

  it should "return correct results for street" in {
    Bet.of("St", List(Number(7)), 1) shouldEqual Right(
      Bet("St", List(Number(7), Number(8), Number(9)), 1)
    )
    Bet.of("St", List(Number(8)), 1) shouldEqual Left("Wrong bet format")
    Bet.of("St", List.empty, 1) shouldEqual Left("Wrong bet format")
    Bet.of("St", List(Number(25), Number(26)), 1) shouldEqual Left(
      "Wrong bet format"
    )
  }

  it should "return correct results for square" in {
    Bet.of("Sq", List(Number(30), Number(29), Number(32), Number(33)), 1) shouldEqual Right(
      Bet("Sq", List(Number(30), Number(29), Number(32), Number(33)), 1)
    )
    Bet.of("Sq", List(Number(25)), 1) shouldEqual Left("Wrong bet format")
    Bet.of("Sq", List(Number(25), Number(35), Number(29), Number(33)), 1) shouldEqual Left(
      "Wrong bet format"
    )
    Bet.of("Sq", List.empty, 1) shouldEqual Left("Wrong bet format")
  }

  it should "return correct results for double street" in {
    Bet.of("DS", List(Number(1)), 1) shouldEqual Right(
      Bet(
        "DS",
        List(Number(1), Number(2), Number(3), Number(4), Number(5), Number(6)),
        1
      )
    )
    Bet.of("DS", List(Number(34)), 1) shouldEqual Left("Wrong bet format")
    Bet.of("DS", List(Number(8)), 1) shouldEqual Left("Wrong bet format")
    Bet.of("DS", List.empty, 1) shouldEqual Left("Wrong bet format")
    Bet.of("DS", List(Number(25), Number(26)), 1) shouldEqual Left(
      "Wrong bet format"
    )
  }

  it should "return correct results for basket" in {
    Bet.of("Ba", List(Number(1)), 1) shouldEqual Right(
      Bet("Ba", List(Number(0), Number(1), Number(2)), 1)
    )
    Bet.of("Ba", List(Number(3)), 1) shouldEqual Right(
      Bet("Ba", List(Number(0), Number(2), Number(3)), 1)
    )
    Bet.of("Ba", List(Number(8)), 1) shouldEqual Left("Wrong bet format")
  }

  it should "return correct results for first four" in {
    Bet.of("FF", List.empty, 1) shouldEqual Right(
      Bet("FF", List(Number(0), Number(1), Number(2), Number(3)), 1)
    )
    Bet.of("FF", List(Number(1)), 1) shouldEqual Left("Wrong bet format")
  }

  it should "return correct results for red" in {
    Bet.of("Re", List.empty, 1) shouldEqual Right(
      Bet(
        "Re",
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
    Bet.of("Re", List(Number(1)), 1) shouldEqual Left("Wrong bet format")
  }

  it should "return correct results for black" in {
    Bet.of("Bl", List.empty, 1) shouldEqual Right(
      Bet(
        "Bl",
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
    Bet.of("Bl", List(Number(1)), 1) shouldEqual Left("Wrong bet format")
  }

  it should "return correct results for even" in {
    Bet.of("Ev", List.empty, 1) shouldEqual Right(
      Bet(
        "Ev",
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
    Bet.of("Ev", List(Number(1)), 1) shouldEqual Left("Wrong bet format")
  }

  it should "return correct results for odd" in {
    Bet.of("Od", List.empty, 1) shouldEqual Right(
      Bet(
        "Od",
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
    Bet.of("Od", List(Number(1)), 1) shouldEqual Left("Wrong bet format")
  }

  it should "return correct results for small" in {
    Bet.of("Sm", List.empty, 1) shouldEqual Right(
      Bet(
        "Sm",
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
    Bet.of("Sm", List(Number(1)), 1) shouldEqual Left("Wrong bet format")
  }

  it should "return correct results for big" in {
    Bet.of("Bi", List.empty, 1) shouldEqual Right(
      Bet(
        "Bi",
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
    Bet.of("Bi", List(Number(1)), 1) shouldEqual Left("Wrong bet format")
  }

  it should "return correct results for dozens" in {
    Bet.of("Do", List(Number(1)), 1) shouldEqual Right(
      Bet(
        "Do",
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
    Bet.of("Do", List(Number(13)), 1) shouldEqual Right(
      Bet(
        "Do",
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
    Bet.of("Do", List(Number(25)), 1) shouldEqual Right(
      Bet(
        "Do",
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
    Bet.of("Do", List(Number(2)), 1) shouldEqual Left("Wrong bet format")
  }

  it should "return correct results for rows" in {
    Bet.of("Ro", List(Number(34)), 1) shouldEqual Right(
      Bet(
        "Ro",
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
    Bet.of("Ro", List(Number(35)), 1) shouldEqual Right(
      Bet(
        "Ro",
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
    Bet.of("Ro", List(Number(36)), 1) shouldEqual Right(
      Bet(
        "Ro",
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
    Bet.of("Ro", List(Number(1)), 1) shouldEqual Left("Wrong bet format")
  }

  it should "return correct result for getResult function" in {
    Bet("Si", List(Number(34)), 1).getResult(Number(34)) shouldEqual 36
    Bet("Sp", List(Number(33), Number(36)), 1)
      .getResult(Number(33)) shouldEqual 18
    Bet("St", List(Number(34), Number(35), Number(36)), 1)
      .getResult(Number(35)) shouldEqual 12
    Bet("Sq", List(Number(26), Number(29), Number(32), Number(35)), 1)
      .getResult(Number(32)) shouldEqual 9
    Bet(
      "DS",
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
    Bet("Ba", List(Number(0), Number(2), Number(3)), 1)
      .getResult(Number(2)) shouldEqual 6
    Bet("FF", List(Number(0), Number(1), Number(2), Number(3)), 1)
      .getResult(Number(2)) shouldEqual 9
    models.Bet("Ev", evenValues, 1).getResult(Number(2)) shouldEqual 2
    models.Bet("Od", oddValues, 1).getResult(Number(1)) shouldEqual 2
    Bet("Re", redValues, 1).getResult(Number(34)) shouldEqual 2
    models.Bet("Do", dozen, 1).getResult(Number(12)) shouldEqual 3
    models.Bet("Ro", row, 1).getResult(Number(35)) shouldEqual 3
  }
}
