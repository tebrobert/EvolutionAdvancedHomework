package dev.codescreen

import dev.codescreen.ChurchBool.True

/**
 * This is a fundamental example of polymorphism usage
 * and a basic example of tagless final encoding
 *
 */
trait ChurchBool {
  // self reference needed to check additional typing requirements
  // and to refer this object in the subexpressions which have their own `this`
  self =>
  /** encoded if(this) ... else ... expression */
  def cond[A](ifTrue: => A, ifFalse: => A): A

  def or(that: => ChurchBool): ChurchBool = cond(True, that)

  def and(that: => ChurchBool): ChurchBool = ???

  def not: ChurchBool = ???

  def toBool: Boolean = ???
}

object ChurchBool {
  def fromBool(b: => Boolean): ChurchBool = ???

  object True extends ChurchBool {
    override def cond[A](ifTrue: => A, ifFalse: => A): A = ifTrue
  }

  object False extends ChurchBool {
    override def cond[A](ifTrue: => A, ifFalse: => A): A = ifFalse
  }
}
