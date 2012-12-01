import fos._

object TypeTest {

  type Constraint = (Type, Type)
  case class TypeError(msg: String) extends Exception(msg)

  def tv(tp: Type): List[String] = tp match {
    case TypeVar(a) => List(a)
    case TypeFun(a, b) => tv(a) ::: tv(b)
    case _ => Nil
  }

  def unify(c: List[Constraint]): Substitution =
    if (c.isEmpty) emptySubst
    else c.head match {
      case (TypeVar(a), TypeVar(b)) if (a == b) => unify(c.tail)
      //   ... To complete ...
      case (a @ TypeVar(v), b) if (!tv(b).exists(_ == v)) =>
        val sub = emptySubst.extend(a, b);
        unify(c.tail.map(p => (sub(p._1), sub(p._2)))).compose(sub)
      case (a, b @ TypeVar(v)) if (!tv(a).exists(_ == v)) =>
        val sub = emptySubst.extend(b, a);
        unify(c.tail.map(p => (sub(p._1), sub(p._2)))).compose(sub)
      case (TypeFun(l1, l2), TypeFun(r1, r2)) => unify((l2, r2) :: (l1, r1) :: c.tail)
      case (t1, t2) =>
        throw TypeError("Could not unify: " + t1 + " with " + t2)
    }
  
  def inferWithoutLet {
    val subst1 = emptySubst.extend(TypeVar("y"), TypeBool).extend(TypeVar("x"), TypeVar("y"))
    val subst2 = emptySubst.extend(TypeVar("y"), TypeNat)
    val subst3 = emptySubst.extend(TypeVar("y"), TypeVar("x")).extend(TypeVar("x"), TypeNat)

    val c: List[Constraint] = List(
      (TypeVar("a2"), TypeFun(TypeVar("a3"), TypeVar("a4"))),
      (TypeVar("a1"), TypeFun(TypeVar("a6"), TypeVar("a5"))),
      (TypeVar("a0"), TypeFun(TypeVar("a1"), TypeVar("a2"))),
      (TypeVar("a1"), TypeFun(TypeVar("a5"), TypeVar("a4"))),
      (TypeVar("a3"), TypeVar("a5")))
    val sub = unify(c)
    println("result = " + sub(TypeVar("a0")))
  }
  
  def instantiateTest() {
    val ts = TypeScheme(List(TypeVar("a"), TypeVar("H")), TypeFun(TypeVar("a"), TypeVar("H")))
    val ts1 = TypeScheme(List(TypeVar("a"), TypeVar("H")), TypeFun(TypeVar("a"), TypeVar("H")))
    
    ts.instantiate
    println(ts1.instantiate)
  }

  def main(args: Array[String]) {
  }
}