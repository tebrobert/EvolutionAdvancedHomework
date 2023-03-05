package dev.codescreen

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.must.Matchers
import ChurchNat.{One, Zero}

class ChurchNatSuite extends AnyFunSuite with Matchers {
  private def nat(i: Int): ChurchNat =
    new ChurchNat {
      def count[N](zero: N)(increment: N => N): N = LazyList.iterate(zero, i + 1)(increment).last
    }

  test("toInt is correct") {
    Zero.toInt mustBe 0
    One.toInt mustBe 1
    Zero.successor.successor.successor.successor.toInt mustBe 4
    (1 to 52).foldLeft(Zero)((n, _) => n.successor).toInt mustBe 52
  }

  test("fromInt is correct") {
    for (x <- List(0, 1, 3, 4, 6, 10, 23, 100))
      ChurchNat.fromInt(x).map(_.toInt) mustBe Some(x)

    for (x <- List(-1, -4, -100))
      ChurchNat.fromInt(x) mustBe None
  }

  test("+ is correct") {
    for (x <- List(0, 1, 3, 4, 6, 10, 23, 100); y <- List(0, 2, 5, 8, 10, 19, 30, 100))
      (nat(x) + nat(y)).toInt mustBe x + y
  }

  test("- is correct") {
    for (x <- List(0, 1, 3, 4, 6, 10, 23, 100); y <- List(0, 2, 5, 8, 10, 19, 30, 100))
      (nat(x) - nat(y)).toInt mustBe (x - y).max(0)
  }

  test("isZero") {
    for (x <- List(0, 1, 3, 4, 6))
      nat(x).isZero mustBe (x == 0)
  }


  test("< is correct") {
    for (x <- List(0, 1, 3, 4, 6, 10, 23, 100); y <- List(0, 2, 5, 8, 10, 19, 30, 100))
      (nat(x) < nat(y)) mustBe x < y
  }

  test("* is correct") {
    for (x <- List(0, 1, 3, 4, 6, 10, 23, 100); y <- List(0, 1, 2, 5, 8, 10, 19))
      (nat(x) * nat(y)).toInt mustBe x * y
  }

  test("exp is correct") {
    for (x <- List(0, 1, 3, 4, 6, 10); y <- List(0, 1, 2, 3))
      BigDecimal((nat(x).exp(nat(y))).toInt) mustBe BigDecimal(x).pow(y)
  }

  test("=== is correct") {
    for (x <- List(0, 1, 3, 4, 6, 10, 23, 31); y <- List(0, 1, 3, 4, 6, 10, 23, 31))
      nat(x) === nat(y) mustBe x == y
  }
}
