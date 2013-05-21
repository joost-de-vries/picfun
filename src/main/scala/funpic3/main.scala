package funpic3
//lets abstract away the dependency on Option frrom Functor
import scala.language.higherKinds //necessary for our abstracting away of Option

trait Functor[+A]{
  //abstract away Option. The concrete subtype Option will give this type attribute the correct value
  type This[+A]
  //the transformation function to be called a Functor
  //equivalent to fmap in Haskell
   def transform[B](f: A => B): This[B]
   
   //the canonical name for this transformation in Scala. Necessary for 'for(i<-someX)yield i' type expressions
  def map[B](f: A => B): This[B] = transform(f)
}

sealed trait Option[+A] extends Functor[A] {
  type This[+A] = Option[A]
  
}
case class Some[+A](a: A) extends Option[A] {
  override def transform[B](f: A => B) = Some(f(a))

}
case object None extends Option[Nothing] {
  override def transform[B](f: Nothing => B) = None

}

object Main extends App {
  //everything still works
  def plusThree = { i: Int => i + 3 }

  val someFive = Some(2) transform { _ + 3 }
  assert(someFive == Some(5))

  val nonePlusThree: Option[Int] = None transform plusThree
  assert(nonePlusThree == None)

  def timesTwo(i: Int) = i * 2
  val someFiveTimesTwo: Option[Int] = for (i <- someFive) yield timesTwo(i)
  val noneTimesTwo: Option[Int] = for (i <- nonePlusThree) yield timesTwo(i)

}