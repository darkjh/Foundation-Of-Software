package fos

import scala.util.parsing.combinator.syntactical.StandardTokenParsers
import scala.util.parsing.input._
import scala.util.parsing.syntax._
import scala.collection.immutable.HashMap

/**
 * This object implements a parser and evaluator for the
 *  simply typed lambda calculus found in Chapter 9 of
 *  the TAPL book.
 */
object SimplyTyped extends StandardTokenParsers {
  lexical.delimiters ++= List("(", ")", "\\", ".", ":", "=", "->", "{", "}", ",", "*", "+", "=>", "|")
  lexical.reserved ++= List("Bool", "Nat", "true", "false", "if", "then", "else", "succ",
    "pred", "iszero", "let", "in", "fst", "snd", "inl", "inr", "of", "case", "fix", "as", "letrec")

  /**
   * Term     ::= SimpleTerm { SimpleTerm }
   */
  def term: Parser[Term] = positioned(
    rep1(simpleTerm) ^^ { case list => (list.head /: list.tail)(App(_, _)) } |
      failure("illegal start of term"))

  /**
   * SimpleTerm ::= "true"
   *               | "false"
   *               | number
   *               | "succ" Term
   *               | "pred" Term
   *               | "iszero" Term
   *               | "if" Term "then" Term "else" Term
   *               | ident
   *               | "\" ident ":" Type "." Term
   *               | "(" Term ")"
   *               | "let" ident ":" Type "=" Term "in" Term
   *               | "{" Term "," Term "}"
   *               | "fst" Term
   *               | "snd" Term
   *               | "inl" Term "as" Type
   *               | "inr" Term "as" Type
   *               | "case" Term "of" "inl" x "=>" t "|" "inr" x "=>" t
   */
  def simpleTerm: Parser[Term] = positioned(
    "(" ~> term <~ ")" |
      ident ^^ { case e => Var(e) } |
      ("\\" ~> ident) ~ (":" ~> tp) ~ ("." ~> term) ^^ { case id ~ tp ~ t => Abs(Var(id), tp, t) } |
      ("let" ~> ident) ~ (":" ~> tp) ~ ("=" ~> term) ~ ("in" ~> term) ^^ { case x ~ tp ~ t1 ~ t2 => App(Abs(Var(x), tp, t2), t1) } |
      value |
      "succ" ~> term ^^ { case e => Succ(e) } |
      "pred" ~> term ^^ { case e => Pred(e) } |
      "iszero" ~> term ^^ { case e => IsZero(e) } |
      ("if" ~> term) ~ ("then" ~> term) ~ ("else" ~> term) ^^ { case cond ~ t1 ~ t2 => If(cond, t1, t2) } |
      ("{" ~> term <~ ",") ~ (term <~ "}") ^^ { case f ~ s => Pair(f, s) } |
      "fst" ~> term ^^ { case p => Fst(p) } |
      "snd" ~> term ^^ { case p => Snd(p) } |
      ("inl" ~> term) ~ ("as" ~> tp) ^^ { case t ~ tp => Inl(t, tp) } |
      ("inr" ~> term) ~ ("as" ~> tp) ^^ { case t ~ tp => Inr(t, tp) } |
      ("case" ~> term <~ "of") ~ ("inl" ~> ident) ~ ("=>" ~> term <~ "|") ~ ("inr" ~> ident) ~ ("=>" ~> term) ^^ {
        case sum ~ inl ~ t1 ~ inr ~ t2 => Case(sum, inl, t1, inr, t2)
      } |
      "fix" ~> term ^^ { case t => Fix(t) } |
      "letrec" ~> ident ~ (":" ~> tp) ~ ("=" ~> term) ~ ("in" ~> term) ^^ { case x ~ tp ~ t1 ~ t2 => App(Abs(Var(x), tp, t2), Fix(Abs(Var(x), tp, t1))) } |
      failure("illegal start of simple term"))

  def value: Parser[Term] = {
    "true" ^^^ True |
      "false" ^^^ False |
      numericValue |
      ("{" ~> value <~ ",") ~ (value <~ "}") ^^ { case f ~ s => Pair(f, s) } |
      ("inl" ~> value) ~ ("as" ~> tp) ^^ { case v ~ tp => Inl(v, tp) } |
      ("inr" ~> value) ~ ("as" ~> tp) ^^ { case v ~ tp => Inr(v, tp) } |
      failure("illegal start of expression")
  }

  def numericDesuger(n: Int): Term = {
    if (n == 0) Zero
    else Succ(numericDesuger(n - 1))
  }

  def numericValue: Parser[Term] = {
    numericLit ^^ { case e => numericDesuger(e.toInt) } |
      "succ" ~> numericValue ^^ { case e => Succ(e) } |
      failure("illegal start of expression")
  }

  /**
   * Type       ::= SimpleType [ "->" Type ]
   */
  //  def pairTp: Parser[Type] = positioned(
  //    simpleTp ~ rep("*" ~> simpleTp) ^^ { case tp ~ list => (tp /: list)(TypePair(_, _)) })
  //    
  //  def sumTp: Parser[Type] = positioned(
  //    rep(simpleTp <~ "+") ~ simpleTp ^^ { case list ~ tp => (list foldRight tp)(TypeSum(_, _)) })

  def typeResolver(s: ~[Type, String], tp: Type): Type = s match {
    case ss ~ op if op == "+" => TypeSum(ss, tp)
    case ss ~ op if op == "*" => TypePair(ss, tp)
  }

  def complexTp: Parser[Type] = positioned(
    rep(simpleTp ~ ("+" | "*")) ~ simpleTp ^^ { case list ~ tp => (list foldRight tp)(typeResolver(_, _)) })

  def simpleTp: Parser[Type] = positioned(
    "Bool" ^^^ TypeBool |
      "Nat" ^^^ TypeNat |
      "(" ~> tp <~ ")")

  def tp: Parser[Type] = positioned(
    rep(complexTp <~ "->") ~ complexTp ^^ { case list ~ tp => (list :\ tp)(TypeFun(_, _)) } |
      failure("illegal start of type"))

  /** Thrown when no reduction rule applies to the given term. */
  case class NoRuleApplies(t: Term) extends Exception(t.toString)

  /** Print an error message, together with the position where it occured. */
  case class TypeError(pos: Position, msg: String) extends Exception(msg) {
    override def toString =
      msg + "\n" + pos.longString
  }

  /** The context is a list of variable names paired with their type. */
  type Context = HashMap[String, Type]

  /** Is the given term a numeric value? */
  def isNumericVal(t: Term): Boolean = t match {
    case Succ(t) => true
    case Pred(t) => true
    case Zero => true
    case _ => false
  }

  /** Is the given term a value? */
  def isValue(t: Term): Boolean =
    t match {
      case Abs(_, _, _) => true
      case True => true
      case False => true
      case Pair(f, s) if isValue(f) && isValue(s) => true
      case Inl(t, _) if isValue(t) => true
      case Inr(t, _) if isValue(t) => true
      case t if isNumericVal(t) => true
      case _ => false
    }

  //  /** Alpha conversion */
  /** no free variables in STLC since they dont pass the type checker */
  //  def alpha0(t: Term, y: String): Term = t match {
  //    // arithetic
  //    case Succ(t) => Succ(alpha0(t, y))
  //    case Pred(t) => Pred(alpha0(t, y))
  //    case IsZero(t) => IsZero(alpha0(t, y))
  //    case If(cond, t, e) => If(alpha0(cond, y), alpha0(t, y), alpha0(e, y))
  //
  //    // extension
  //    case Fst(t) => Fst(alpha0(t, y))
  //    case Snd(t) => Snd(alpha0(t, y))
  //    case Pair(f, s) => Pair(alpha0(f, y), alpha0(s, y))
  //
  //    // lambda
  //    case App(a, b) => App(alpha0(a, y), alpha0(b, y))
  //    //case Abs(Var(x), t) => Abs(Var(y + "'"), alpha0(t, y))
  //    case Abs(x, tp, t1) if x.v != y => Abs(x, tp, alpha0(t1, y))
  //    case Var(x) if x == y => Var(x + "'")
  //    case Var(x) if x != y => Var(x)
  //    case t => t
  //  }
  //
  //  def alpha(t: Term): Term = {
  //    t match {
  //      case Abs(Var(y), tp, t) => Abs(Var(y + "'"), tp, alpha0(t, y))
  //    }
  //  }

  /** Substitution */
  def subst(t: Term, x: String, s: Term): Term = t match {
    // arithmetic
    case IsZero(t) => IsZero(subst(t, x, s))
    case Succ(t) => Succ(subst(t, x, s))
    case Pred(t) => Pred(subst(t, x, s))
    case If(cond, t, e) => If(subst(cond, x, s), subst(t, x, s), subst(e, x, s))

    // extension -- pair
    case Fst(t) => Fst(subst(t, x, s))
    case Snd(t) => Snd(subst(t, x, s))
    case Pair(fst, snd) => Pair(subst(fst, x, s), subst(snd, x, s))

    // extension -- sum
    case Inl(t, tp) => Inl(subst(t, x, s), tp)
    case Inr(t, tp) => Inr(subst(t, x, s), tp)
    case Case(sum, inl, t1, inr, t2) => Case(subst(sum, x, s), inl, subst(t1, x, s), inr, subst(t2, x, s))

    // lambda
    case Var(y) if (y == x) => s
    case Var(y) if (y != x) => t
    case Abs(Var(y), _, t1) if (y == x) => t
    case Abs(Var(y), tp, t1) if (y != x) => Abs(Var(y), tp, subst(t1, x, s))
    //    case Abs(Var(y), tp, t1) if (y != x && !FV(s).exists(_.v == y)) => Abs(Var(y), tp, subst(t1, x, s))
    //    case Abs(Var(y), _, t1) if (y != x && FV(s).exists(_.v == y)) => subst(alpha(t), x, s)
    case App(t1, t2) => App(subst(t1, x, s), subst(t2, x, s))

    case e => e
  }

  /** Free variables in a term */
  /** no free variables in STLC since they dont pass the type checker */
  //  def FV(t: Term): List[Var] = t match {
  //    // arithmetic
  //    case Zero => Nil
  //    case True => Nil
  //    case False => Nil
  //    case If(cond, t, e) => FV(cond) ::: FV(t) ::: FV(e)
  //    case Pred(t) => FV(t)
  //    case Succ(t) => FV(t)
  //    case IsZero(t) => FV(t)
  //
  //    // extension
  //    case Fst(t) => FV(t)
  //    case Snd(t) => FV(t)
  //    case Pair(f, s) => FV(f) ::: FV(s)
  //
  //    // lambda
  //    case a: Var => List(a)
  //    case Abs(Var(x), _, t1) => FV(t1).filter(_ != Var(x))
  //    case App(t1, t2) => FV(t1) ::: FV(t2)
  //  }

  /** Call by value reducer. */
  def reduce(t: Term): Term = t match {
    // arithmetic
    case If(True, t2, t3) => t2
    case If(False, t2, t3) => t3
    case If(t1, t2, t3) => If(reduce(t1), t2, t3)
    case IsZero(Zero) => True
    case IsZero(Succ(tm)) => False // if isNumericVal(tm) => False
    case Pred(Zero) => Zero
    case Pred(Succ(tm)) => tm // if isNumericVal(tm) => tm
    case IsZero(tm) => IsZero(reduce(tm))
    case Pred(tm) => Pred(reduce(tm))
    case Succ(tm) => Succ(reduce(tm))

    // extension -- pair
    case Fst(Pair(f, s)) if isValue(f) && isValue(s) => f
    case Snd(Pair(f, s)) if isValue(f) && isValue(s) => s
    case Fst(t) => Fst(reduce(t))
    case Snd(t) => Snd(reduce(t))
    case Pair(f, s) if !isValue(f) => Pair(reduce(f), s)
    case Pair(f, s) if !isValue(s) => Pair(f, reduce(s))

    // extension -- sum
    case Case(Inl(v, tp), inl, t1, inr, t2) if isValue(v) => subst(t1, inl, v)
    case Case(Inr(v, tp), inl, t1, inr, t2) if isValue(v) => subst(t2, inl, v)
    case Case(sum, inl, t1, inr, t2) if !isValue(sum) => Case(reduce(sum), inl, t1, inr, t2)
    case Inl(t, tp) if !isValue(t) => Inl(reduce(t), tp)
    case Inr(t, tp) if !isValue(t) => Inr(reduce(t), tp)

    // lambda
    //    case App(Abs(x,_, t1), t2) if isValue(t2) => subst(t1, x.v, t2)
    //    case App(t1, t2) if isValue(t1) => App(reduce(t1), t2)
    case App(Abs(x, _, t1), t2) if isValue(t2) => subst(t1, x.v, t2)
    case App(t1, t2) if isValue(t1) => App(t1, reduce(t2))
    case App(t1, t2) => App(reduce(t1), t2)

    // fix point
    case Fix(a @ Abs(x, tp, t2)) => subst(t2, x.v, Fix(a))
    case Fix(t) => Fix(reduce(t))

    case _ => throw NoRuleApplies(t)
  }

  /**
   * Returns the type of the given term <code>t</code>.
   *
   *  @param ctx the initial context
   *  @param t   the given term
   *  @return    the computed type
   */
  def typeof(ctx: Context, t: Term): Type = t match {

    // arithmetic
    case True => TypeBool
    case False => TypeBool
    case Zero => TypeNat
    case Pred(e) if typeof(ctx, e) == TypeNat => TypeNat
    case Succ(e) if typeof(ctx, e) == TypeNat => TypeNat
    case IsZero(e) if typeof(ctx, e) == TypeNat => TypeBool
    case If(cond, t, e) if typeof(ctx, cond) == TypeBool && typeof(ctx, t) == typeof(ctx, e) => typeof(ctx, t)

    // extension -- pair
    case Pair(f, s) =>
      (typeof(ctx, f), typeof(ctx, s)) match {
        case (tf: Type, ts: Type) => TypePair(tf, ts)
      }
    case Fst(p) => typeof(ctx, p) match {
      case TypePair(tf, ts) => tf
      case _ => throw TypeError(t.pos, "type error at: " + t + "\ncontext: " + ctx.toString)
    }
    case Snd(p) => typeof(ctx, p) match {
      case TypePair(tf, ts) => ts
      case _ => throw TypeError(t.pos, "type error at: " + t + "\ncontext: " + ctx.toString)
    }

    // extension -- fix point
    case Fix(t) => typeof(ctx, t) match {
      case TypeFun(tp1, tp2) if (tp1 == tp2) => tp1
      case _ => throw TypeError(t.pos, "type error at: " + t + "\ncontext: " + ctx.toString)
    }

    // extension -- sum
    case Inl(t, tp) => tp match {
      case TypeSum(l, r) => typeof(ctx, t) match {
        case tt: Type if tt == l => tp
        case _ => throw TypeError(t.pos, "type error at: " + t + "\ncontext: " + ctx.toString)
      }
    }
    case Inr(t, tp) => tp match {
      case TypeSum(l, r) => typeof(ctx, t) match {
        case tt: Type if tt == r => tp
        case _ => throw TypeError(t.pos, "type error at: " + t + "\ncontext: " + ctx.toString)
      }
    }
    case Case(sum, inl, t1, inr, t2) => typeof(ctx, sum) match {
      case t @ TypeSum(l, r) => {
        val tp1 = typeof(ctx + (inl -> l), t1)
        val tp2 = typeof(ctx + (inr -> r), t2)
        if (tp1 == tp2) tp1
        else throw TypeError(t.pos, "type error at: " + t + "\ncontext: " + ctx.toString)
      }
    }

    // lambda
    case x: Var if ctx.contains(x.v) => ctx.get(x.v).get
    case Abs(x, tp, t) => typeof(ctx.+((x.v, tp)), t) match {
      case tt: Type => TypeFun(tp, tt)
    }
    case App(l, r) => (typeof(ctx, l), typeof(ctx, r)) match {
      case (TypeFun(t11, t12), tr: Type) if t11 == tr => t12
      case (TypeFun(t11, t12), tr: Type) if t11 != tr => throw TypeError(t.pos, "parameter type mismatch: expected " + t11 + ", found " + tr)
      case _ => throw TypeError(t.pos, "type error at: " + t + "\ncontext: " + ctx.toString)
    }

    case _ => throw TypeError(t.pos, "type error at: " + t + "\ncontext: " + ctx.toString)
  }

  /**
   * Returns a stream of terms, each being one step of reduction.
   *
   *  @param t      the initial term
   *  @param reduce the evaluation strategy used for reduction.
   *  @return       the stream of terms representing the big reduction.
   */
  def path(t: Term, reduce: Term => Term): Stream[Term] =
    try {
      var t1 = reduce(t)
      Stream.cons(t, path(t1, reduce))
    } catch {
      case NoRuleApplies(_) =>
        Stream.cons(t, Stream.empty)
    }

  def main(args: Array[String]): Unit = {
    //    val tokens = new lexical.Scanner(StreamReader(new java.io.InputStreamReader(System.in)))

    //    val input = "(\\x:Nat->Bool. (\\y:Nat.(x y))) (\\x:Nat.(iszero x)) 0"
    //        val input = "case (inr false as Nat + Bool) of inl a => succ a | inr a => 1"
    //        val input = "inr 1 as Nat + Bool"
    //    val input = "(\\y:Nat->Nat. (\\f:Nat->Nat. \\y:Nat. f y) (\\x:Nat. y succ(x)))"
    //    val input = "(\\x:Nat. \\y:Nat. iszero (y x))"
    //        val input = "let a:Nat = 2 in let b:Bool = false in if b then {b, a} else {b, succ a}"
    //    val input = "fst {(\\x:Nat. succ x) 1, (\\x:Nat. iszero x) 0}"
    //    val input = "(\\y:Nat*Bool. \\x:Nat*Bool. {x, {1,y}} )"
//        val input = "(fix (\\f:Nat->Bool. " +
//        		"\\x:Nat. " +
//        		"if iszero x then true " +
//        		"else if iszero (pred x) then false " +
//        		"else  f (pred (pred x)))) " +
//        		"succ succ 0"

    val input = "letrec f: Nat->Nat =" +
      "\\x:Nat. " +
      "if iszero x then 0 " +
      "else succ( f (pred x) )" +
      "in f 3"

    val tokens = new lexical.Scanner(input)

    phrase(term)(tokens) match {
      case Success(trees, _) =>
        try {
          println("for input :\n" + input)
          println("parsed: " + trees)
          println("result:")
          println("typed: " + typeof(new HashMap[String, Type](), trees))
          for (t <- path(trees, reduce))
            println(t)
        } catch {
          case tperror => Console.err.println(tperror.toString)
        }
      case e =>
        println(e)
    }
  }
}
