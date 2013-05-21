package funpic22
import scala.language.higherKinds 

trait Functor[+A]{
  type This[+A]

  def transform[B](f: A => B): This[B]
   
  def map[B](f: A => B): This[B] = transform(f)
}
trait Monad[+A]{
  type This[+A]
    def transform2[B](f: A => This[B]): This[B]

    //the canonical name for this in Scala
    //called >>= or bind in Haskell
  def flatMap[B](f: A => This[B]): This[B] = transform2(f)
}
sealed trait Option[+A] extends Functor[A] with Monad[A]{
  type This[+A] = Option[A]
  
}
case class Some[+A](a: A) extends Option[A] {
  override def transform[B](f: A => B) = Some(f(a))

  override def transform2[B](f: A => Option[B]) = f(a)
}
case object None extends Option[Nothing] {
  override def transform[B](f: Nothing => B) = None
  override def transform2[B](f: Nothing => Option[B]) = None

}

object Main extends App {
  def plusThree = { i: Int => i + 3 }

  val someFive = Some(2) transform { _ + 3 }

  val nonePlusThree: Option[Int] = None transform plusThree

  def timesTwo(i: Int) = i * 2
  val someFiveTimesTwo: Option[Int] = for (i <- someFive) yield timesTwo(i)
  val noneTimesTwo: Option[Int] = for (i <- nonePlusThree) yield timesTwo(i)

  def plus(i: Int, j: Int) = i + j
  //to be able to use the for keyword in a 'chained' way our Option needs to implement def flatMap[B](f:A=>Option[B]):Option[B]
  //this is called being a Monad
  //note how we can keep going with these transformations because of this trick
  //this is called being composable
  val someFiveTimesTwoPlusNoneTimesTwo = for {
    i <- someFiveTimesTwo
    j <- noneTimesTwo
  } yield plus(i, j)
  assert(someFiveTimesTwoPlusNoneTimesTwo == None)

  val someFivePlusNone = Some(5) flatMap { i => None map { j => plus(i, j) } }
  assert(someFivePlusNone == None)
  //the former is what the scala compiler produces from the following
  val someFivePlusNone2 = for {
    i <- Some(5)
    j <- None
  } yield plus(i, j)
  assert(someFivePlusNone == someFivePlusNone2)
}