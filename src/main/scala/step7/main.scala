package step7

object Main extends App {
  def isRude(s:String)="buttock"==s
  //use map with console IO in scala: getLines is an Iterator[String]
val censuredConsole=for( ln <- io.Source.stdin.getLines if(!isRude(ln)) ) yield ln 

//nothing has been read yet

for(p<-censuredConsole) println( s"received '$p'")

}