import fos._

object TypeTest {
  
  def main(args: Array[String]) {
    val subst1 = emptySubst.extend(TypeVar("y"), TypeBool).extend(TypeVar("x"), TypeVar("y"))
    val subst2 = emptySubst.extend(TypeVar("y"), TypeNat)
   
    println(subst1(TypeFun(TypeVar("x"), TypeVar("x"))))
  }
}