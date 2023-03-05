package dev.codescreen

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.must.Matchers

class ChurchBoolSuite extends AnyFunSuite with Matchers {
  // an obfuscated version, don't use it in your own code
  private val fromBool =
    (0 to 1).map(i => (i % 2 == 0) -> new ChurchBool {
      def cond[A](ifTrue: => A, ifFalse: => A): A = List(() => ifTrue, () => ifFalse)(i)()
    }).toMap

  test("fromBool") {
    ChurchBool.fromBool(true).cond("true", "false") mustEqual "true"
    ChurchBool.fromBool(false).cond("true", "false") mustEqual "false"
  }

  test("toBool") {
    for (x <- List(true, false)) fromBool(x).toBool mustEqual x
  }

  test("basic or operations") {
    for (x <- List(true, false); y <- List(true, false))
      fromBool(x).or(fromBool(y)).toBool mustEqual x || y
  }

  test("basic and operations") {
    for (x <- List(true, false); y <- List(true, false))
      fromBool(x).and(fromBool(y)).toBool mustEqual x && y
  }

  test("basic not operations") {
    for (x <- List(true, false))
      ChurchBool.fromBool(x).not.toBool mustEqual !x
  }

  private def error() = throw new RuntimeException("too eager initialization")

  test("`or` should be lazy on second element") {
    ChurchBool.fromBool(true).or(error()).toBool mustEqual true
  }

  test("`and` should be lazy on second element"){
    ChurchBool.fromBool(false).and(error()).toBool mustEqual false
  }

  test("condition in from bool should not be evaluated eagerly") {
    val bool = ChurchBool.fromBool(error())
  }

  test("'not' should not evaluate boolean") {
    val bool = ChurchBool.fromBool(error()).not
  }
}
