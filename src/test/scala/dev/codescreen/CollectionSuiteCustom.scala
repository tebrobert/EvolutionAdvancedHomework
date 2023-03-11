package dev.codescreen

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.must.Matchers

class CollectionSuiteCustom extends AnyFunSuite with Matchers {
  def bad = throw new Exception("Not lazy")

  def cols(elems: Vector[Int]) =
    List(
      Collection(elems: _*),
      Collection(elems: _*).concat(bad),
      Collection(elems: _*).concat(bad).concat(bad),
    )

  def positive = (_: Int) > 0

  test("concat works and is lazy") {
    val preElems = Vector(5, 6)
    val preCol = Collection(preElems: _*)
    val elems = Vector(7, 8, -999)

    cols(elems).map(preCol.concat(_).takeWhile(positive).toVector
      mustEqual preElems.concat(elems).takeWhile(positive)
    )
  }

  test("map works and is lazy") {
    val elems = Vector(7, 8, -999)
    val f = (_: Int) * 11

    cols(elems).map(_.map(f).takeWhile(positive).toVector
      mustEqual elems.map(f).takeWhile(positive)
    )
  }

  test("filter works and is lazy") {
    val elems = Vector(7, 8, -999)
    val f = (_: Int)%2 != 0

    cols(elems).map(_.filter(f).takeWhile(positive).toVector
      mustEqual elems.filter(f).takeWhile(positive)
    )
  }

  test("zipWithIndex works and is lazy") {
    val elems = Vector(7, 8, -999)
    val positiveNumber = (_: (Int, Int))._2 > 0

    cols(elems).map(_.zipWithIndex.takeWhile(positiveNumber).toVector
      mustEqual elems.zipWithIndex.takeWhile(positiveNumber)
    )
  }

  test("flatMap works and is lazy") {
    val elems = Vector(7, 8, -999)
    val f = (x: Int) => Vector(x * 11, x * 111)
    val F = f.andThen(Collection(_: _*))

    cols(elems).map(_.flatMap(F).takeWhile(positive).toVector
      mustEqual elems.flatMap(f).takeWhile(positive)
    )
  }

  test("collectFirst works") {
    val definedAt1: PartialFunction[Int, Int] = { case 7 => 10000000 }

    Collection().collectFirst(definedAt1) mustEqual None
    Collection(-1).collectFirst(definedAt1) mustEqual None

    val elems = Vector(7, 8, -999)
    val definedAt7: PartialFunction[Int, Int] = { case 7 => 10000000 }
    val definedAt8: PartialFunction[Int, Int] = { case 8 => 10000000 }

    cols(elems).map(_.collectFirst(definedAt7)
      mustEqual elems.collectFirst(definedAt7)
    )
    cols(elems).map(_.collectFirst(definedAt8)
      mustEqual elems.collectFirst(definedAt8)
    )
  }
}

