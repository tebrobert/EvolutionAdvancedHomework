package dev.codescreen

import dev.codescreen.BinTree._

sealed trait BinTree[+A] {
  def map[B](f: A => B): BinTree[B] =
    this match {
      case Node(value, left, right) =>
        Node(f(value), left.map(f), right.map(f))
      case Nil => Nil
    }

  /** traverse tree depth first in order
   * first there are all values from the left subtree
   * then the value in the current node
   * and then all values in the right subtree */
  def toVector: Vector[A] =
    this match {
      case Node(value, left, right) =>
        left.toVector ++ Vector(value) ++ right.toVector
      case Nil => Vector()
    }

  private def compareRec[A1 >: A](
    yTree: BinTree[A1], path: String
  )(implicit eq: Equiv[A1]): Option[String] =
    (this, yTree) match {
      case (Nil, Nil) => None
      case (Node(xVal, xLeft, xRight), Node(yVal, yLeft, yRight))
        if eq.equiv(xVal, yVal) =>
        (for {
          _ <- xLeft.compareRec(yLeft, path + "L").toLeft()
          _ <- xRight.compareRec(yRight, path + "R").toLeft()
        } yield ())
          .left.toOption
      case _ => Some(path)
    }

  /** compare two binary trees using given equivalence
   * return None if binary tree are equivalent or path consisting of letters "L" and "R"
   * that leads to the some node where values are different
   * or one of the subtrees is `Nil` while other is `Node` */
  def compare[A1 >: A](tree: BinTree[A1])(implicit eq: Equiv[A1]): Option[String] =
    compareRec(tree, "")
}

object BinTree {
  case class Node[+A](value: A, left: BinTree[A], right: BinTree[A]) extends BinTree[A]

  // try to analyze why BinTree[Nothing] works here
  case object Nil extends BinTree[Nothing]
}
