package fos

/** Two-phase inferencer, first collect constraints, then solve them. */
class TwoPhaseInferencer extends TypeInferencers {
  import Type._

  type Constraint = (Type, Type)

  val noConstraints: List[Constraint] = Nil
  case class TypingResult(tpe: Type, c: List[Constraint])

  /**
   * Type <code>t</code> in <code>env</code> and return its type and a
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
      case _ => throw TypeError("Nat expected")
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
      else TypingResult(t1.instantiate, noConstraints)
      // else TypingResult(t1, noConstraints)
    }
    case Abs(v, tp, t) => {
      tp match {
        case EmptyType => {
          val newTypeVar = TypeVar(freshTypeBase)
          val ts = TypeScheme(Nil, newTypeVar)
          val sub = collect((v, ts) :: env, t)
          TypingResult(TypeFun(newTypeVar, sub.tpe), sub.c)
        }
        case tt => {
          val boundTp = toType(tt)
          val listTypeVars = tv(boundTp)
          val sub = collect((v, TypeScheme(listTypeVars, toType(tt))) :: env, t)
          TypingResult(TypeFun(toType(tt), sub.tpe), sub.c)
        }
      }
    }
    case App(t1, t2) => {
      val sub1 = collect(env, t1)
      val sub2 = collect(env, t2)
      val fresh = TypeVar(freshTypeName(freshTypeBase))
      TypingResult(fresh, (sub1.tpe, TypeFun(sub2.tpe, fresh)) :: sub1.c ++ sub2.c)
    }
    
    case Let(x, v, t) => {
      val tr_v = collect(env, v)
      val subst = unify(tr_v.c)
      val pt_v = subst(tr_v.tpe)
      val newEnv = subst(env)
      val tvNotInEnv = tv(pt_v) filter ((v: TypeVar) => lookup(newEnv, v.name) == null)
      val ts_v = TypeScheme(tvNotInEnv, pt_v)
      
      collect((x, ts_v) :: newEnv, t)
    }
  }

  /**
   */
  def unify(c: List[Constraint]): Substitution =
    if (c.isEmpty) emptySubst
    else c.head match {
      case (a, b) if (a == b) => unify(c.tail)
      case (a @ TypeVar(v), b) if (!tv(b).exists(_.name.equals(v))) =>
        val sub = emptySubst.extend(a, b);
        unify(c.tail.map(p => (sub(p._1), sub(p._2)))).compose(sub)
      case (a, b @ TypeVar(v)) if (!tv(a).exists(_.name.equals(v))) =>
        val sub = emptySubst.extend(b, a);
        unify(c.tail.map(p => (sub(p._1), sub(p._2)))).compose(sub)
      case (TypeFun(l1, l2), TypeFun(r1, r2)) => unify( (l2, r2)::(l1, r1):: c.tail) 
      case (t1, t2) =>
        throw TypeError("Could not unify: " + t1 + " with " + t2)
    }
  
  def tv(tp: Type): List[TypeVar] = tp match {
    case a @ TypeVar(_) => List(a)
    case TypeFun(a, b) => tv(a) ::: tv(b)
    case _ => Nil
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
