package test

object test extends App {
	
  import fos.Type._
  import fos._
  
  def testTypeVar() = {
    val ctx1: Context = List(("A", "a"), ("B", "b"))
    println(typeOf(Var("d"), ctx1))
  }
  
  testTypeVar()
}