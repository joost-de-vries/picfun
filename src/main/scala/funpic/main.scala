package funpic

sealed trait Option[+A]  { }

case class Some[A](a:A) extends Option[A]

case object None extends Option[Nothing]

object Main extends App{
  val eenLetter = Some("a")
  val niets = None
  
  def string(b:Boolean):Option[String]=  if(b) Some("a") else None
  val no=string(false)
  
  assert(no == None)
  assert(no.isInstanceOf[Option[String]])
  
  val yes=string(true)
  assert(yes.isInstanceOf[Option[String]])
}