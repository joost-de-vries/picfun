package funpic4
trait Functor[+A]{
  //the transformation that a Functor needs to support
  //equivalent to fmap in Haskell
   def transform[B](f: A => B): Option[B]
   //the canonical name for this transformation in Scala. Necessary for 'for(i<-someX)yield i' type expressions
  def map[B](f: A => B): Option[B] = transform(f)
}
trait Applicative[+A]{
  //apply the wrapped function to this
  def transform3[B](f:Option[A=>B]):Option[B]
}
trait Monad[+A]{
    def transform2[B](f: A => Option[B]): Option[B]

    //the canonical name for this in Scala
    //called >>= or bind in Haskell
  def flatMap[B](f: A => Option[B]): Option[B] = transform2(f)
}
sealed trait Option[+A] extends Functor[A] with Applicative[A] with Monad[A]{
  //needed to be able to do for(i<-Some(3) if(i%2))yield i
  def filter(p:A=>Boolean):Option[A]
  def withFilter(p:A=>Boolean):Option[A]=filter(p)
}
case class Some[+A](a: A) extends Option[A] {
  override def transform[B](f: A => B) = Some(f(a))

  override def transform2[B](f: A => Option[B]) = f(a)
  
  override  def transform3[B](f:Option[A=>B]):Option[B]= f.transform[B]({f2 => f2(a)})
  
  def filter(p:A=>Boolean):Option[A]= if(p(a)) Some(a) else None
}
case object None extends Option[Nothing] {
  override def transform[B](f: Nothing => B) = None
  override def transform2[B](f: Nothing => Option[B]) = None
  override def transform3[B](f:Option[Nothing=>B]):Option[B]=None
  override def filter(p:Nothing=>Boolean)=None
}
object Console  extends Functor[String] with Monad[String]{
  override def transform[B](f: String => B) = None
  override def transform2[B](f: String => Option[B]) = None
  
}
object Main extends App {
  def half(i:Int) = if(i%2==0) Some(i/2) else None
val plus=(i:Int)=>(j:Int)=> i+j
  
  assert(Some(5).transform2(half) == None)
  val same=Some(5).flatMap(half)
  
   
  for{i<-Some(3) if( i>2)
    j<-Some(4) if (j>3)} yield plus(i)(j)
}