package step5
//a similar transformation that is not as crucial as the other two
import language.higherKinds

trait Functor[+A] {
  def transform[B](f: A => B): Option[B]
  def map[B](f: A => B): Option[B] = transform(f)
}

trait Applicative[+A] {
  type This[+A]
  //apply the wrapped function to this
  //this contrived looking function makes it possible to work with functions with more arguments
  //as we'll see
  //if you don't do OO the signature is: This[A=>B] => This[A]=>This[B]
  def transform3[B](f: This[(A) => B]): This[B]
  
  //the canonical name for this in scala is apply. this method will be invoked if we do Some(1)(f)
  //this is <$> in Haskells
  def apply[B](f: This[(A) => B]): This[B]=transform3(f)
  
}

trait Monad[+A] {
  type This[+A]
  def transform2[B](f: A => This[B]): This[B]

  def flatMap[B](f: A => This[B]): This[B] = transform2(f)
}

sealed trait Option[+A] extends Functor[A] with Applicative[A] with Monad[A] {
  type This[+A] = Option[A]
}
case class Some[+A](a: A) extends Option[A] {
  override def transform[B](f: A => B) = Some(f(a))

  override def transform2[B](f: A => Option[B]) = f(a)

  override def transform3[B](f: Option[A => B]): Option[B] = f.transform[B]({ f2 => f2(a) })
}
case object None extends Option[Nothing] {
  override def transform[B](f: Nothing => B) = None
  override def transform2[B](f: Nothing => Option[B]) = None
  override def transform3[B](f: Option[Nothing => B]): Option[B] = None
}

object Main extends App {
  def plusThree = { i: Int => i + 3 }

  //the transform3 method is the equivalent of the <*> function of Applicative in Haskell
  //or the Applicative.apply function in libs.functional in Play
  //http://www.playframework.com/documentation/api/2.1.1/scala/index.html#play.api.libs.functional.Applicative
  assert(Some(2).transform3(Some(plusThree)) == Some(5))

  //an applicative can work with a higher arity function like plus
  val plus = (i: Int) => (j: Int) => i + j
  val someThreePlus = Some(3).transform3(Some(plus)) //partially applied
  val someFour = Some(1).transform3(someThreePlus)
  assert(Some(4) == someFour)
  
  //or, more succinctly:
  val alsoSomeFour = Some(1)( Some(3)(Some(plus)) )
  
  
  assert(Some(4)==alsoSomeFour)

}