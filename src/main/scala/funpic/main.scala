package funpic
//the basics: an object to signal whether there's an outcome or not
sealed trait Option[+A]

//this is called Just(a) in Haskell
case class Some[A](a: A) extends Option[A]

//the type Nothing is a subtype of all other types. That's why None
//doesn't need to know about A
case object None extends Option[Nothing]

object Main extends App {
  //a function that has sometimes a defined output
  def half(i: Int) = if (i % 2 == 0) Some(i / 2) else None

  val anInt = half(4)
  val none = half(5)

  assert(none == None)
  assert(none.isInstanceOf[Option[Int]])

  assert(anInt.isInstanceOf[Option[Int]])
}