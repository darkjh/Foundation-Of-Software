package fos

/** Two-phase inferencer, first collect constraints, then solve them. */
class TwoPhaseInferencer extends TypeInferencers {
  import Type._

  type Constraint = (Type, Type)

  val noContraints: List[Constraint] = Nil
  case class TypingResult(tpe: Type, c: List[Constraint])

  /**
   * Type <code>t</code> in <code>env</code> and return its type and a
   *  constraint list.
   */
  def collect(env: Env, t: Term): TypingResult = TypingResult(TypeNat, Nil)
  //    t match {
  //    case Var(x) =>
  //      val t1 = lookup(env, x)
  //      if (t1 == null)
  //        throw TypeError("Unknown variable " + x)
  //      TypingResult(t1.instantiate, noContraints)
  //  //   ... To complete ... 
  //  }

  /**
   */
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
      case (TypeFun(l1, l2), TypeFun(r1, r2)) => unify( (l2, r2)::(l1, r1):: c.tail) 
      case (t1, t2) =>
        throw TypeError("Could not unify: " + t1 + " with " + t2)
    }
  
  def tv(tp: Type): List[String] = tp match {
    case TypeVar(a) => List(a)
    case TypeFun(a, b) => tv(a) ::: tv(b)
  }

  override def typeOf(t: Term): Type = try {
    val TypingResult(tp, c) = collect(Nil: Env, t)
    val s = unify(c)
    s(tp)
  } catch {
    case TypeError(msg) =>
      Console.println("type error: " + msg)
      null
  }

}
