package step2
//next step: let's add a handy transformation to our Option
//so that we can use existing functions that know nothing about our Option class
//and apply them anyway to it, whether it wraps a meaningful value or not

//something that implements this handy transformation is called a Functor
trait Functor[+A]{
  //the transformation function to be called a Functor
  //equivalent to fmap in Haskell
  //too bad that we still have a dependency on Option
   def transform[B](f: A => B): Option[B]
   
   //the canonical name for this transformation in Scala. Necessary for 'for(i<-someX)yield i' type expressions
  def map[B](f: A => B): Option[B] = transform(f)
}

sealed trait Option[+A] extends Functor[A] {

}
case class Some[+A](a: A) extends Option[A] {
  override def transform[B](f: A => B) = Some(f(a))

}
case object None extends Option[Nothing] {
  override def transform[B](f: Nothing => B) = None

}

object Main extends App {
  //a function that knows nothing about our Option class
  def plusThree = { i: Int => i + 3 }

  //but we can apply it anyway to instances of Option
  //because our Option implements def transform[B](f:A=>B):Option[B]
  //equivalent to haskell fmap Just(2) (+3)
  val someFive = Some(2) transform { _ + 3 }
  assert(someFive == Some(5))

  val nonePlusThree: Option[Int] = None transform plusThree
  assert(nonePlusThree == None)

  //now we can take part in a scala for comprehension because of the scala map function
  def timesTwo(i: Int) = i * 2
  val someFiveTimesTwo: Option[Int] = for (i <- someFive) yield timesTwo(i)
  val noneTimesTwo: Option[Int] = for (i <- nonePlusThree) yield timesTwo(i)

 }
