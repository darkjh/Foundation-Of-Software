package fos

import scala.util.parsing.combinator.syntactical.StandardTokenParsers
import scala.util.parsing.input._
import scala.util.parsing.syntax._

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
    //"0" ^^^ Zero() |
    numericValue |
    failure("illegal start of expression")
  }

  def numericValue: Parser[Term] = {
    numericLit ^^ { case e => Numeric(e.toInt) } |
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

  /** Is the given term a numeric value? */
  def isNumericVal(t: Term): Boolean = t match {
    //   ... To complete ... 
    case _ => false
  }

  /** Is the given term a value? */
  def isValue(t: Term): Boolean = t match {
    //   ... To complete ... 
    case _ => false
  }

  /** Call by value reducer. */
  def reduce(t: Term): Term = t match {
    //   ... To complete ... 
    case _ =>
      throw NoRuleApplies(t)
  }

  /**
   * Returns the type of the given term <code>t</code>.
   *
   *  @param ctx the initial context
   *  @param t   the given term
   *  @return    the computed type
   */
  def typeof(ctx: Context, t: Term): Type = t match {
    case True | False =>
      TypeBool
    //   ... To complete ... 
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
    val input = "if iszero (\\x:Bool.\\y:Nat->Bool->Nat. x (y 0) 5) 5 then true else false"
    val tokens = new lexical.Scanner(input)
    phrase(term)(tokens) match {
      //      case Success(trees, _) =>
      //        try {
      //          println("typed: " + typeof(Nil, trees))
      //          for (t <- path(trees, reduce))
      //            println(t)
      //        } catch {
      //          case tperror => println(tperror.toString)
      //        }
      case e =>
        println(e)
    }
  }
}
