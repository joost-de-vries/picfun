package funpic3
import scala.language.higherKinds

trait Functor[+A] {
  def transform[B](f: A => B): Option[B]
  def map[B](f: A => B): Option[B] = transform(f)
}

trait Applicative[+A] {
  //higherkind type 
  type This[+A]
  //apply the wrapped function to this
  def transform3[B](f: This[(A) => B]): This[B]
}
sealed trait Option[+A] extends Functor[A] with Applicative[A] {
  type This[+A] = Option[A]
  def transform2[B](f: A => Option[B]): Option[B]

  def flatMap[B](f: A => Option[B]): Option[B] = transform2(f)
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
  val someThreePlus=Some(3).transform3(Some(plus)) //partially applied
  val someFour = Some(1).transform3(someThreePlus)
  assert(Some(4) == someFour)

  val x = for (ln <- io.Source.stdin.getLines) yield (ln)
}