package dev.codescreen

import dev.codescreen.ChurchBool._

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

  def and(that: => ChurchBool): ChurchBool =
    cond(that, False)

  def not: ChurchBool =
    new ChurchBool {
      override def cond[A](ifTrue: => A, ifFalse: => A): A =
        self.cond(ifFalse, ifTrue)
    }

  def toBool: Boolean =
    cond(true, false)
}

object ChurchBool {
  def fromBool(b: => Boolean): ChurchBool =
    new ChurchBool {
      override def cond[A](ifTrue: => A, ifFalse: => A): A =
        if (b) ifTrue else ifFalse
    }

  object True extends ChurchBool {
    override def cond[A](ifTrue: => A, ifFalse: => A): A = ifTrue
  }

  object False extends ChurchBool {
    override def cond[A](ifTrue: => A, ifFalse: => A): A = ifFalse
  }
}
