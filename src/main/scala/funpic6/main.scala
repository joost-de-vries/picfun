package funpic6
//we need to implement one more function to take part in scala for comprehensions
//and as a result we don't have to consider nullpointers 
//while we reuse the generic scala for comprehension syntax
import language.higherKinds

trait Functor[+A] {
  type This[+A]

  def transform[B](f: A => B): This[B]

  def map[B](f: A => B): This[B] = transform(f)
}
trait Applicative[+A] {
  type This[+A]

  def transform3[B](f: This[A => B]): This[B]
}
trait Monad[+A] {
  type This[+A]
  def transform2[B](f: A => This[B]): This[B]

  def flatMap[B](f: A => This[B]): This[B] = transform2(f)
}
trait Filterable[+A] {
  type This[+A]
  //the canonical name
  //to be able to support the 'if' part of for(i<-Some(5) if(even(i)) )yield i
  def withFilter(p: A => Boolean): Option[A]
}
//so this is all you need to be able to take part in a scala for comprehension
//arguably the essential powerful feature of functional programming
trait ForComprehensable[+A] extends Functor[A] with Monad[A] with Filterable[A]

sealed trait Option[+A] extends ForComprehensable[A] with Applicative[A] {
  type This[+A] = Option[A]
}
case class Some[+A](a: A) extends Option[A] {
  override def transform[B](f: A => B) = Some(f(a))

  override def transform2[B](f: A => Option[B]) = f(a)

  override def transform3[B](f: Option[A => B]): Option[B] = f.transform[B]({ f2 => f2(a) })

  override def withFilter(p: A => Boolean): Option[A] = if (p(a)) Some(a) else None
}
case object None extends Option[Nothing] {
  override def transform[B](f: Nothing => B) = None
  override def transform2[B](f: Nothing => Option[B]) = None
  override def transform3[B](f: Option[Nothing => B]): Option[B] = None
  override def withFilter(p: Nothing => Boolean) = None
}

object Main extends App {
  val plus = (i: Int) => (j: Int) => i + j

  //now we can use the if construction
  for {
    i <- Some(3) if (i > 2)
    j <- Some(4) if (j > 3)
  } yield plus(i)(j)
}