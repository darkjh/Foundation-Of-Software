package fos

import scala.util.parsing.combinator.syntactical.StandardTokenParsers
import scala.util.parsing.input._
import scala.util.parsing.syntax._

/**
 * This object implements a parser and evaluator for the
 *  untyped lambda calculus found in Chapter 5 of
 *  the TAPL book.
 */
object Untyped extends StandardTokenParsers {
  lexical.delimiters ++= List("(", ")", "\\", ".")
  lexical.reserved += "apply"
  import lexical.Identifier

  /**
   * Term     ::= AbsOrVar { AbsOrVar }
   */

  /**
   *  t := x
   *  	|  "\\" x "." t
   *  	|  t t
   *    |  "(" t ")"
   */

  //  term := v					* variable
  //       |  "\\" x "." term"		* abstraction
  //       |  appl				* application
  //       |  "(" term ")"
  //       
  //  appl := term (term)+ 		* 2 or more terms

  /* RH's re-organized version */
  def v: Parser[Var] = ident ^^ { case e => Var(e) }
  def abs: Parser[Abs] = ("\\" ~> v) ~ ("." ~> term) ^^ { case x ~ t => Abs(x, t) }
  def pTerm: Parser[Term] = "(" ~> term <~ ")"
  def genTerm: Parser[Term] = { v | abs | pTerm }
  def term: Parser[Term] = {
    rep1(genTerm) ^^ { case list => (list.head /: list.tail)(App(_, _)) } |
      failure("illegal start of term")
  }

  /* JH's original version */
  //  def v: Parser[Var] = ident ^^ { case e => Var(e) }
  //  def abs: Parser[Abs] = ("\\" ~> v) ~ ("." ~> term) ^^ { case x ~ t => Abs(x, t) }
  //  def absOrVar: Parser[Term] = v | abs
  //  def appl: Parser[List[Term]] = rep(absOrVar)
  //  def term: Parser[Term] = {
  //    ("(" ~> term) ~ (")" ~> appl) ^^ { case t ~ list => (t /: list)(App(_, _)) } |
  //      v ~ appl ^^ { case v ~ list => (v /: list) (App(_, _)) } |
  //      abs ~ appl ^^ { case v ~ list => (v /: list)(App(_, _)) } |
  //      failure("illegal start of term")
  //  }

  /** Term 't' does not match any reduction rule. */
  case class NoRuleApplies(t: Term) extends Exception(t.toString)

  /**
   * Normal order (leftmost, outermost redex first).
   *
   *  @param t the initial term
   *  @return  the reduced term
   */
  def reduceNormalOrder(t: Term): Term = t match {
    case App(Abs(x, t1), t2) => subst(t1, x.v, t2)
    case App(t1, t2) => App(reduceNormalOrder(t1), t2)
    case Abs(x, t1) => Abs(x, reduceNormalOrder(t1))
    case _ => throw NoRuleApplies(t)
  }

  def alpha(t: Term): Term = {
    def alpha0(t: Term, y: String): Term = t match {
      case App(a, b) => App(alpha0(a, y), alpha0(b, y))
      //case Abs(Var(x), t) => Abs(Var(y + "'"), alpha0(t, y))
      case Abs(x, t1) if x.v != y => Abs(x, alpha0(t1, y))
      case Var(x) if x == y => Var(x + "'")
      case Var(x) if x != y => Var(x)
      case t => t
    }

    t match {
      case Abs(Var(y), t) => Abs(Var(y + "'"), alpha0(t, y))
    }
  }

  def subst(t: Term, x: String, s: Term): Term = t match {
    case Var(y) if (y == x) => s
    case Var(y) if (y != x) => t
    case Abs(Var(y), t1) if (y == x) => t
    case Abs(Var(y), t1) if (y != x && !FV(s).exists(_.v == y)) => Abs(Var(y), subst(t1, x, s))
    case Abs(Var(y), t1) if (y != x && FV(s).exists(_.v == y)) => subst(alpha(t), x, s)
    case App(t1, t2) => App(subst(t1, x, s), subst(t2, x, s))
  }

  def FV(t: Term): List[Var] = t match {
    case a: Var => List(a)
    case Abs(Var(x), t1) => FV(t1).filter(_ != Var(x))
    case App(t1, t2) => FV(t1) ::: FV(t2)
  }

  def isval(t: Term): Boolean = t match {
    case t: Abs => true
    case t: Var => false
    case t => isval(reduceCallByValue(t))
  }

  /** Call by value reducer. */
  def reduceCallByValue(t: Term): Term = t match {
    case App(Abs(x, t1), t2) if isval(t2) => subst(t1, x.v, t2)
    case App(t1, t2) if isval(t1) => App(reduceCallByValue(t1), t2)
    case _ => throw NoRuleApplies(t)
  }

  /**
   * Returns a stream of terms, each being one step of reduction.
   *
   *  @param t      the initial term
   *  @param reduce the method that reduces a term by one step.
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
    //    val input = " \\y. ((\\x.x) y)"
    //    val input = " \\y. \\y. y y"
    //    val input = "(\\t. \\f. f) v w"
    val input = "(\\b. \\c. b c (c (\\t. \\f. f) (\\t. \\f. t))) (\\t. \\f. t) v"
    //    val input = "(\\x. x) ( (\\x. x) \\z. (\\x. x) z)"
    //    val input = "x"
    val tokens = new lexical.Scanner(input)
    phrase(term)(tokens) match {
      case Success(trees, _) =>
        println("input: " + input)
        println("tree: " + trees)
        println("normal order: ")
        for (t <- path(trees, reduceNormalOrder))
          println(t)

        println("call-by-value: ")
        for (t <- path(trees, reduceCallByValue))
          println(t)

      case e =>
        println(e)
    }
  }
}