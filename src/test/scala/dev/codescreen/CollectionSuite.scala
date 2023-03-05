package dev.codescreen

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.must.Matchers

class CollectionSuite extends AnyFunSuite with Matchers {
  def take[A](c: Collection[A], n: Int) =
    c.zipWithIndex.takeWhile(_._2 < n).map(_._1)

  test("empty") {
    Collection.empty.toVector mustEqual Vector.empty
  }

  test("single") {
    Collection.single(123).toVector mustEqual Vector(123)
  }

  test("simple map") {
    Collection(1 to 30: _*).map(_ + 2).toVector mustEqual 3.to(32).toVector
  }

  test("simple filter") {
    Collection(1 to 100: _*).filter(_ % 3 == 1).toVector mustEqual 1.to(100).by(3).toVector
  }


  test("simple drop") {
    Collection(1 to 32: _ *).drop(10).toVector mustEqual (11 to 32).toVector
  }

  test("simple dropWhile") {
    Collection(1 to 32: _ *).dropWhile(_ <= 14).toVector mustEqual (15 to 32).toVector
  }

  test("simple zipWithIndex") {
    Collection("abcdef": _*).zipWithIndex.toVector mustEqual "abcdef".zipWithIndex
  }

  test("simple takeWhile") {
    Collection("abcdefg__dsfsd": _*).takeWhile(_.isLetter).toVector mustEqual "abcdefg".toVector
  }

  test("collection concat") {
    Collection("abcd": _*).concat(Collection("efgh": _*)).toVector mustEqual "abcdefgh".toVector
  }

  test("lazy concat") {
    take(Collection("abcd": _*).concat(throw new RuntimeException("concatenation should be lazy")), 3).toVector mustEqual "abc".toVector
  }

  test("inifinite concat") {
    lazy val infinite: Collection[String] = Collection.single("a").concat(infinite)
    take(infinite, 10).toVector mustEqual Vector.fill(10)("a")
  }
}

