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
  lexical.delimiters ++= List("(", ")", "\\", ".", ":", "=", "->", "{", "}", ",", "*")
  lexical.reserved ++= List("Bool", "Nat", "true", "false", "if", "then", "else", "succ",
    "pred", "iszero", "let", "in", "fst", "snd")

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
   */
  def simpleTerm: Parser[Term] = positioned(
    "(" ~> term <~ ")" |
    ident ^^ { case e => Var(e) } |
    ("\\" ~> ident) ~ (":" ~> tp) ~ ("." ~> term) ^^ { case id ~ tp ~ t => Abs(Var(id), tp, t) } | 
    value |
    "succ" ~> term ^^ { case e => Succ(e) } |
    "pred" ~> term ^^ { case e => Pred(e) } |
    "iszero" ~> term ^^ { case e => IsZero(e) } |
    ("if" ~> term) ~ ("then" ~> term) ~ ("else" ~> term) ^^ { case cond ~ t1 ~ t2 => If(cond, t1, t2) } |
    failure("illegal start of simple term"))

  def value: Parser[Term] = {
    "true" ^^^ True |
    "false" ^^^ False |
    numericValue |
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

  def simpleTp: Parser[Type] = positioned(
    "Bool" ^^^ TypeBool |
    "Nat" ^^^ TypeNat |
    "(" ~> tp <~ ")")
  def tp: Parser[Type] = positioned(
    rep(simpleTp <~ "->") ~ simpleTp ^^ { case list ~ tp => (list :\ tp)(TypeFun(_, _)) } |
    failure("illegal start of type"))

  //   ... To complete ... 

  /** Thrown when no reduction rule applies to the given term. */
  case class NoRuleApplies(t: Term) extends Exception(t.toString)

  /** Print an error message, together with the position where it occured. */
  case class TypeError(pos: Position, msg: String) extends Exception(msg) {
    override def toString =
      msg + "\n" + pos.longString
  }

  /** The context is a list of variable names paired with their type. */
  type Context = List[(String, Type)]
  type Context0 = HashMap[String, Type]

  /** Is the given term a numeric value? */
  def isNumericVal(t: Term): Boolean = t match {
    case Succ(t) => true
    case Pred(t) => true
    // case t: Numeric => true
    case _ => false
  }

  /** Is the given term a value? */
  def isValue(t: Term): Boolean = t match {
    case t: Abs => true
    case t: Var => false
    case _ => false
  }
  
  /** Alpha conversion */
  def alpha(t: Term): Term = {
    def alpha0(t: Term, y: String): Term = t match {
      case App(a, b) => App(alpha0(a, y), alpha0(b, y))
      //case Abs(Var(x), t) => Abs(Var(y + "'"), alpha0(t, y))
      case Abs(x, tp, t1) if x.v != y => Abs(x, tp, alpha0(t1, y))
      case Var(x) if x == y => Var(x + "'")
      case Var(x) if x != y => Var(x)
      case t => t
    }

    t match {
      case Abs(Var(y), tp, t) => Abs(Var(y + "'"), tp, alpha0(t, y))
    }
  }
  
  /** Substitution */
  def subst(t: Term, x: String, s: Term): Term = t match {
    case Var(y) if (y == x) => s
    case Var(y) if (y != x) => t
    case Abs(Var(y), _, t1) if (y == x) => t
    case Abs(Var(y), tp, t1) if (y != x && !FV(s).exists(_.v == y)) => Abs(Var(y), tp, subst(t1, x, s))
    case Abs(Var(y), _, t1) if (y != x && FV(s).exists(_.v == y)) => subst(alpha(t), x, s)
    case App(t1, t2) => App(subst(t1, x, s), subst(t2, x, s))
  }
  
  /** Free variables in a term */
  def FV(t: Term): List[Var] = t match {
    case a: Var => List(a)
    case Abs(Var(x), _, t1) => FV(t1).filter(_ != Var(x))
    case App(t1, t2) => FV(t1) ::: FV(t2)
  }

  /** Call by value reducer. */
  def reduce(t: Term): Term = t match {
    // arithmetic
    // TODO simplify rules, no check needed
    case If(True, t2, t3) => t2
    case If(False, t2, t3) => t3
    case If(t1, t2, t3) => val v = reduce(t1); If(v, t2, t3)
    case IsZero(Zero) => True
    case IsZero(Succ(tm)) if (isNumericVal(tm) == true) => False
    case Pred(Zero) => Zero
    case Pred(Succ(tm)) if (isNumericVal(tm) == true) => tm
    case IsZero(tm) => val v = reduce(tm); IsZero(v)
    case Pred(tm) => val v = reduce(tm); Pred(v)
    case Succ(tm) => val v = reduce(tm); Succ(v)
    
    // lambda
    case App(Abs(x,_, t1), t2) if isValue(t2) => subst(t1, x.v, t2)
    case App(t1, t2) if isValue(t1) => App(reduce(t1), t2)
    case _ => throw NoRuleApplies(t)
  }
  
  /** Get the type for a variable from the context */
  def getType(ctx: Context, v: String): Type = {
	if (ctx.isEmpty) null
    else if (ctx.head._1 == v) ctx.head._2
    else getType(ctx.tail, v)
  }
  
  def addVar(ctx: Context0, x: Var, t: Type): Context0 = {
    if (!ctx.contains(x.v)) ctx.+((x.v, t))
    else ctx.+((x.v + "'", t))
  }
  
  /**
   * Returns the type of the given term <code>t</code>.
   *
   *  @param ctx the initial context
   *  @param t   the given term
   *  @return    the computed type
   */
  def typeof(ctx: Context0, t: Term): Type = t match {
    // case n: Numeric => TypeNat // TODO nums should be de-sugered when parsing
    case True | False => TypeBool
    case Zero => TypeNat
    case Pred(e) if typeof(ctx, e) == TypeNat => TypeNat
    case Succ(e) if typeof(ctx, e) == TypeNat => TypeNat
    case IsZero(e) if typeof(ctx, e) == TypeNat => TypeBool
    case If(cond, t, e) if typeof(ctx, cond) == TypeBool && typeof(ctx, t) == typeof(ctx,e) => typeof(ctx, t)
    case x: Var if ctx.contains(x.v) => ctx.get(x.v).get
    case Abs(x, tp, t) if typeof(ctx.+((x.v, tp)) , t).isInstanceOf[Type] => TypeFun(tp, typeof(ctx.+((x.v, tp)), t))
    case App(l, r) if typeof(ctx, l).isInstanceOf[TypeFun] && typeof(ctx, r).isInstanceOf[Type] =>
      typeof(ctx, l) match {
        case TypeFun(t11, t12) if (t11 == typeof(ctx, r)) => t12
      }
    case _ => throw TypeError(t.pos, "type error")
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
    // val tokens = new lexical.Scanner(StreamReader(new java.io.InputStreamReader(System.in)))
    val input = "if iszero 2 then true else false"
//    val input = "(\\x: Nat. \\x: Bool. if x then 1 else 2) 1"
    val tokens = new lexical.Scanner(input)
    phrase(term)(tokens) match {
      case Success(trees, _) =>
        try {
          println("parsed: " + trees)
          println("typed: " + typeof(new HashMap[String, Type](), trees))
          for (t <- path(trees, reduce))
            println(t)
        } catch {
          case tperror => println(tperror.toString)
        }
      case e =>
        println(e)
    }
  }
}
