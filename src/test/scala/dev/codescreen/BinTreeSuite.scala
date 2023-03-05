package dev.codescreen

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.must.Matchers

import scala.reflect.ClassTag

class BinTreeSuite extends AnyFunSuite with Matchers {
  private def tree[A: ClassTag](x: Any): BinTree[A] = x match {
    case null => BinTree.Nil
    case Some(value: A) => BinTree.Node(value, BinTree.Nil, BinTree.Nil)
    case (l, value: A) => BinTree.Node(value, tree(l), BinTree.Nil)
    case (value: A, r) => BinTree.Node(value, BinTree.Nil, tree(r))
    case (l, value: A, r) => BinTree.Node(value, tree(l), tree(r))
  }

  private def treeRange(start: Int, stop: Int, step: Int = 1): BinTree[Int] =
    if (start == stop) BinTree.Nil else {
      val next = treeRange(start + step, stop, step)
      BinTree.Node(start, next, next)
    }

  test("map is correct") {
    tree[Int](null).map(_ + 1) mustBe tree[Int](null)
    tree[String](Some("3")).map(x => (x.toInt + 2).toString) mustBe tree[String](Some("5"))
    treeRange(1, 4).map(_ * 3) mustBe treeRange(3, 12, 3)
  }

  test("toVector is correct") {
    tree[Int](null).toVector mustBe Vector()
    tree[Int]((1, (2, (3, null)))).toVector mustBe Vector(1, 2, 3)
    tree[Int]((((Some(1), 2), 3), 4), 5).toVector mustBe Vector(1, 2, 3, 4, 5)
    treeRange(1, 4).toVector mustBe Vector(3, 2, 3, 1, 3, 2, 3)
  }

  test("compare is correct") {
    tree[Boolean](null).compare(tree[Boolean](Some(true))) must contain("")
    tree[Boolean](null).compare(tree[Boolean](null)) mustBe empty
    tree[Boolean]((true, Some(true))).compare(tree[Boolean](Some(true), true)) must contain oneOf("L", "R")
    treeRange(1, 4).compare(treeRange(1, 4)) mustBe empty
    tree[Boolean](true, (Some(true), false)).compare(tree[Boolean]((true, (Some(false), false)))) must contain("RL")
  }
}