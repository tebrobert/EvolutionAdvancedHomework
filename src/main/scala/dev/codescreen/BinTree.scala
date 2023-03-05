package dev.codescreen

sealed trait BinTree[+A] {
  def map[B](f: A => B): BinTree[B] = ???

  /** traverse tree depth first in order
   * first there are all values from the left subtree
   * then the value in the current node
   * and then all values in the right subtree */
  def toVector: Vector[A] = ???

  /** compare two binary trees using given equivalence
   * return None if binary tree are equivalent or path consisting of letters "L" and "R"
   * that leads to the some node where values are different
   * or one of the subtrees is `Nil` while other is Node` */
  def compare[A1](tree: BinTree[A1])(implicit eq: Equiv[A1]): Option[String] = ???
}

object BinTree {
  case class Node[+A](value: A, left: BinTree[A], right: BinTree[A]) extends BinTree[A]

  // try to analyze why BinTree[Nothing] works here
  case object Nil extends BinTree[Nothing]
}
