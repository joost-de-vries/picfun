package step5a
//in the previous step applicative looked a bit unintuitive because
//the wrapped function came in the end
//here we'll do it the other way round
import language.higherKinds

trait Functor[+A] {
  def transform[B](f: A => B): Option[B]
  def map[B](f: A => B): Option[B] = transform(f)
}

trait Applicative[+A] extends Functor[A] {
  type This[+A]

  
  def transform4[B](f:This[A=>B]):This[B]
  //can't define an 'apply' for this because of type erasure in java generics
  //def apply[B](f:This[A=>B]):This[B]=transform4(f)  
  
}
trait ForwardApplicative[F<:(C=>B),+A] extends Applicative[A]{
  //the non-OO signature is: This[A=>B] => This[A]=>This[B]
  def transform3(oc: This[C]):This[B]  
  //the canonical name for this in scala is apply
  def apply(oc: This[C]):This[B]  =transform3(oc)
  
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

  override def transform3[A<:(C=>B),B,C](oc: Option[C]):Option[B]=flatMap(f2=> oc.map(f2.asInstanceOf[C=>B] ))
  override def transform4[B](f:This[A=>B]):This[B]=f.map(f2=> f2(a))
  }
case object None extends Option[Nothing] {
  override def transform[B](f: Nothing => B) = None
  override def transform2[B](f: Nothing => Option[B]) = None
  
  def transform3[A<:(C=>B),B,C](oc: Option[C]):Option[B]=None
  override def transform4[B](f:This[Nothing=>B]):This[B]=None

}

object Main extends App {
  def plusThree = { i: Int => i + 3 }

  //the transform3 method is the equivalent of the <*> function of Applicative in Haskell
  //or the Applicative.apply function in libs.functional in Play
  //http://www.playframework.com/documentation/api/2.1.1/scala/index.html#play.api.libs.functional.Applicative
  assert(Some(2).transform3(Some(plusThree)) == Some(5))

  //an applicative can work with a higher arity function like plus
  val plus = (i: Int) => (j: Int) => i + j

  //'backward': the wrapped function comes last
  val someFour = Some(1)(Some(3)(Some(plus)))
  assert(Some(4) == someFour)
  

  //'forward':the wrapped function comes first
  val alsoSomeFour = Some(plus)(Some(1))(Some(3))
  
  
  assert(Some(4)==alsoSomeFour)

}