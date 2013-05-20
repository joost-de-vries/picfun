package funpic2
trait Functor[+A]{
  //the transformation that a Functor needs to support
  //equivalent to fmap in Haskell
   def transform[B](f: A => B): Option[B]
   //the canonical name for this transformation in Scala. Necessary for 'for(i<-someX)yield i' type expressions
  def map[B](f: A => B): Option[B] = transform(f)
}
sealed trait Option[+A] extends Functor[A] {

  def transform2[B](f: A => Option[B]): Option[B]

  def flatMap[B](f: A => Option[B]): Option[B] = transform2(f)
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

  //you can apply a function that doesn't know anything about our Option class to instances of Option
  //because our Option implements def transform[B](f:A=>B):Option[B]
  //equivalent to haskell fmap Just(2) (+3)
  val someFive = Some(2) transform { _ + 3 }
  assert(someFive == Some(5))

  val nonePlusThree: Option[Int] = None transform plusThree
  assert(nonePlusThree == None)

  //to be able to use the Scala 'for' keyword with 'yield' our Option needs to implement a function def map[B](f:A=>B):Option[B]
  //this is called a for comprehension
  def timesTwo(i: Int) = i * 2
  val someFiveTimesTwo: Option[Int] = for (i <- someFive) yield timesTwo(i)
  val noneTimesTwo: Option[Int] = for (i <- nonePlusThree) yield timesTwo(i)

  def plus(i: Int, j: Int) = i + j
  //to be able to use the for keyword in a 'chained' way our Option needs to implement def flatMap[B](f:A=>Option[B]):Option[B]
  //note how we can keep going with these transformations
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