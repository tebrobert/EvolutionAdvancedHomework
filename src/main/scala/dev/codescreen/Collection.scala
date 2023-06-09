// This is an advanced task
// It's optional so solve it so try it last
package dev.codescreen

import dev.codescreen.Collector.Transformation

import scala.annotation.tailrec
import scala.collection.{Factory, mutable}

/** also can be called visitor, reducer or iteratee
 * visits element one by one, transforming state */
// note that you can add some methods here if they are need for implementation of the `Collection` methods
trait Collector[-Element, State, +Result] {
  // self reference needed to check additional typing requirements
  // and to refer this object in the subexpressions which have their own `this`
  self =>
  /** the initial state of the consumer */
  def init: State

  /** accept single element, can return early result, or next state */
  def consume(prev: State, element: Element): Either[Result, State]

  /** final result if there are no mo elements */
  def result(state: State): Result

  def contramap[A](convert: A => Element): SameState[A] =
    (prev: State, element: A) =>
      self.consume(prev, convert(element))

  def filtered[E1 <: Element](f: E1 => Boolean): SameState[E1] =
    (prev: State, element: E1) =>
      if (f(element)) self.consume(prev, element)
      else Right(prev)

  def dropWhile[E1 <: Element](f: E1 => Boolean) = new Collector[E1, Option[State], Result] {
    def init: Option[State] = None

    def consume(prev: Option[State], element: E1): Either[Result, Option[State]] = prev match {
      case Some(prev) => self.consume(prev, element).map(Some(_))
      case None if !f(element) => self.consume(self.init, element).map(Some(_))
      case _ => Right(None)
    }

    def result(state: Option[State]): Result = self.result(state.getOrElse(self.init))
  }

  def drop(count: Int) = new Collector[Element, Either[Int, State], Result] {
    def init = Left(count)

    override def consume(prev: Either[Int, State], element: Element): Either[Result, Either[Int, State]] = prev match {
      case Left(k) if k > 0 => Right(Left(k - 1))
      case _ => self.consume(prev.getOrElse(self.init), element).map(Right(_))
    }

    def result(state: Either[Int, State]): Result = self.result(state.getOrElse(self.init))
  }

  def takeWhile[E1 <: Element](f: E1 => Boolean) =
    new Collector[E1, State, Result] {
      def init: State =
        self.init
      def consume(prev: State, element: E1): Either[Result, State] =
        if (f(element)) self.consume(prev, element)
        else Left(self.result(prev))
      def result(state: State): Result =
        self.result(state)
    }

  def contraZipWithIndex[A] =
    new Collector[A, (State, Int), Result] {
      override def init: (State, Int) =
        (self.init, 0)
      override def consume(prevWithIndex: (State, Int), element: A): Either[Result, (State, Int)] =
        prevWithIndex match { case (prev, index) =>
          self.consume(prev, (element, index).asInstanceOf[Element])
            .map((_, index + 1))
        }
      override def result(stateWithIndex: (State, Int)): Result =
        stateWithIndex match {case (state, _) => self.result(state) }
    }

  def concat(secondCollection: => Collection[Element]) =
    new Collector[Element, State, Result] {
      override def init: State =
        self.init
      override def consume(prev: State, element: Element): Either[Result, State] =
        self.consume(prev, element)
      override def result(collectedFirst: State) =
        secondCollection.accept(self.from(collectedFirst))
    }

  def contraFlatMap[A](f: A => Collection[Element]) =
    new Collector[A, State, Result] {
      override def init: State =
        self.init
      override def consume(prev: State, element: A): Either[Result, State] =
        f(element).accept(self.keepStateFrom(prev))
      override def result(state: State): Result =
        self.result(state)
    }

  /** utilitary local class for consumer transformation with the same state management */
  abstract class SameState[X] extends Collector[X, State, Result] {
    def init: State = self.init

    def result(state: State): Result = self.result(state)
  }

  private def from(initState: State) =
    new Collector[Element, State, Result] {
      override def init: State =
        initState
      override def consume(prev: State, element: Element): Either[Result, State] =
        self.consume(prev, element)
      override def result(state: State): Result =
        self.result(state)
    }

  private def keepStateFrom(initState: State) =
    new Collector[Element, State, Either[Result, State]] {
      override def init: State =
        initState
      override def consume(prev: State, element: Element): Either[Either[Result, State], State] =
        self.consume(prev, element).left.map(Left(_))
      override def result(state: State): Either[Result, State] =
        Right(state)
    }
}

object Collector {
  /** also called transducer or enumeratee */
  trait Transformation[-A, +B] {
    def transform[S1, Result](collector: Collector[B, S1, Result]): Collector[A, _, Result]
  }


  // advanced code ahead
  // this is a trick to define polymorphic lambdas in Scala 2
  def Transformation[B] = new TransBuilder[B]

  class TransBuilder[B] {
    type MagicState
    type MagicResult

    abstract class TransLambda[-A] extends Transformation[A, B] {
      def apply(collector: Collector[B, MagicState, MagicResult]): Collector[A, _, MagicResult]

      def transform[S1, Result](collector: Collector[B, S1, Result]): Collector[A, _, Result] =
        this.apply(collector.asInstanceOf[Collector[B, MagicState, MagicResult]]).asInstanceOf[Collector[A, _, Result]]
    }

    def apply[A](lambda: TransLambda[A]): Transformation[A, B] = lambda
  }
}

/** also called enumerator
 * a collection of elements */
// Generally most of this methods should be implemented through a special transformation of Collector
// after you defined a suitable method in the Collector trait there are two approaches
// you can create new instance of Collection manually as in method `drop`
// or you can apply some parametric magic as in method `dropWhile`
trait Collection[+A] {
  self =>
  def accept[State, Result](collector: Collector[A, State, Result]): Result

  /** convert collection to collection of the same size of function results */
  def map[B](f: A => B): Collection[B] =
    via(Transformation[B](_.contramap(f)))

  /** keep only values that satisfies predicate */
  def filter(predicate: A => Boolean): Collection[A] =
    via(Transformation[A](_.filtered(predicate)))

  /** keep the longest prefix of values that satisfies predicate */
  def takeWhile(predicate: A => Boolean): Collection[A] =
    via(Transformation[A](_.takeWhile(predicate)))

  /** returns the first value for which partial function is defined */
  def collectFirst[B](f: PartialFunction[A, B]): Option[B] =
    dropWhile(!f.isDefinedAt(_))
      .zipWithIndex
      .takeWhile { case (_, index) => index == 0 }
      .map { case (num, _) => f(num) }
      .toList.headOption


  /** returns pair of elements with their indices starting at 0 */
  def zipWithIndex: Collection[(A, Int)] =
    via(Transformation[(A, Int)](_.contraZipWithIndex[A]))

  /** lazily concatenates two collections, */
  //"lazy" means the `collection` expression should not be evaluated until all elements of this collection are off
  def concat[A1 >: A](collection: => Collection[A1]): Collection[A1] =
    via(Transformation[A1](_.concat(collection)))


  /** should work as a normal collection flatMap
   *  returns the concatenation of all collections, given by f applied to elements of this collection */
  def flatMap[B](f: A => Collection[B]): Collection[B] =
    via(Transformation[B](_.contraFlatMap(f)))

  def via[B](trans: Transformation[A, B]): Collection[B] = new Collection[B] {
    def accept[State, Result](collector: Collector[B, State, Result]): Result = self.accept(trans.transform(collector))
  }

  def to[C](factory: Factory[A, C]): C = accept(new Collector[A, mutable.Builder[A, C], C] {
    def init: mutable.Builder[A, C] = factory.newBuilder

    def consume(prev: mutable.Builder[A, C], element: A): Either[C, mutable.Builder[A, C]] = Right(prev.addOne(element))

    def result(state: mutable.Builder[A, C]): C = state.result()
  })

  def drop(count: Int): Collection[A] = new Collection[A] {
    def accept[State, Result](collector: Collector[A, State, Result]): Result = self.accept(collector.drop(count))
  }

  def dropWhile(f: A => Boolean): Collection[A] = via(Transformation[A](_.dropWhile(f)))


  def toList: List[A] = to(List)

  def toVector: Vector[A] = to(Vector)

}

object Collection {
  def apply[A](elements: A*): Collection[A] = new Collection[A] {
    override def accept[State, Result](collector: Collector[A, State, Result]): Result = {
      val it = elements.iterator

      @tailrec def loop(state: State): Result =
        if (it.hasNext) collector.consume(state, it.next()) match {
          case Left(earlyReturn) => earlyReturn
          case Right(nextState) => loop(nextState)
        } else collector.result(state)

      loop(collector.init)
    }
  }

  // you can redefine this as an object
  def empty: Collection[Nothing] =
    Collection()

  def single[A](element: A): Collection[A] =
    Collection(element)
}



