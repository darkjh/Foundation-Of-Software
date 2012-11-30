package fos

/** Two-phase inferencer, first collect constraints, then solve them. */
class TwoPhaseInferencer extends TypeInferencers {
  import Type._

  type Constraint = (Type, Type)

  val noConstraints: List[Constraint] = Nil
  case class TypingResult(tpe: Type, c: List[Constraint])

  /** Type <code>t</code> in <code>env</code> and return its type and a
   *  constraint list.
   */
  def collect(env: Env, t: Term): TypingResult = t match {
    case True => TypingResult(TypeBool, noConstraints)
    case False => TypingResult(TypeBool, noConstraints)
    case Zero => TypingResult(TypeNat, noConstraints)
    
    case Pred(tm) => collect(env, tm) match {
      case TypingResult(tp, cons) => TypingResult(TypeNat, (tp, TypeNat) :: cons)
      case _ => throw TypeError("Nat expected")
    }
    case Succ(tm) => collect(env, tm) match {
      case TypingResult(tp, cons) => TypingResult(TypeNat, (tp, TypeNat) :: cons)
      case _ => throw TypeError("Nat expected")
    }
    case IsZero(tm) => collect(env, tm) match {
      case TypingResult(tp, cons) => TypingResult(TypeBool, (tp, TypeNat) :: cons)
      case _ => throw TypeError("Bool expected")
    }
    case If(c, t, e) => {
      val tc = collect(env, c)
      val tt = collect(env, t)
      val te = collect(env, e)
      TypingResult(tt.tpe, (tt.tpe, te.tpe) :: (tc.tpe, TypeBool) :: tc.c ++ tt.c ++ te.c)
    }
    case Var(x) => {
      val t1 = lookup(env, x)
      if (t1 == null) throw TypeError("Unknown variable " + x)
      // else TypingResult(t1.instantiate, noConstraints)
      else TypingResult(t1, noConstraints)
    }
    case Abs(v, tp, t) => {
      tp match {
        case EmptyType => {
          val fresh = TypeVar(freshTypeName)
          val sub = collect((v, fresh) :: env, t)
          TypingResult(TypeFun(fresh, sub.tpe), sub.c)
        }
        case tt => {
          val sub = collect((v, tree2type(tt)) :: env, t)
          TypingResult(TypeFun(tree2type(tt), sub.tpe), sub.c)
        }
      }
    }
    case App(t1, t2) => {
      val sub1 = collect(env, t1)
      val sub2 = collect(env, t2)
      val fresh = TypeVar(freshTypeName)
      TypingResult(fresh, (sub1.tpe, TypeFun(sub2.tpe, fresh)) :: sub1.c ++ sub2.c)
    }
  }

  /**
   */
  def unify(c: List[Constraint]): Substitution =
    if (c.isEmpty) emptySubst
    else c.head match {
      case (TypeVar(a), TypeVar(b)) if (a == b) => unify(c.tail)

  //   ... To complete ... 
      case (t1, t2) =>
        throw TypeError("Could not unify: " + t1 + " with " + t2)
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
