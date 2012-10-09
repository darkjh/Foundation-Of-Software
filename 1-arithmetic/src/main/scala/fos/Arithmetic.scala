package fos

import scala.util.parsing.combinator.syntactical.StandardTokenParsers
import scala.util.parsing.input._
import scala.util.parsing.syntax._

case class NoRuleException(msg: String) extends RuntimeException(msg)

/**
 * This object implements a parser and evaluator for the NB
 *  language of booleans and numbers found in Chapter 3 of
 *  the TAPL book.
 */
object Arithmetic extends StandardTokenParsers {
  lexical.reserved ++= List("true", "false", "0", "if", "then", "else", "succ", "pred", "iszero")

  import lexical.NumericLit

  /**
   * Expr ::= 'true'
   * | 'false'
   * | 'if' Expr 'then' Expr 'else' Expr
   * | '0'
   * | 'succ' Expr
   * | 'pred' Expr
   * | 'iszero' Expr
   */
  def Expr: Parser[Term] = {
    Value |
      "iszero" ~> Expr ^^ { case e => IsZero(e) } |
      "succ" ~> Expr ^^ { case e => Succ(e) } |
      "pred" ~> Expr ^^ { case e => Pred(e) } |
      ("if" ~> Expr) ~ ("then" ~> Expr) ~ ("else" ~> Expr) ^^ { case cond ~ t1 ~ t2 => If(cond, t1, t2) } |
      failure("illegal start of expression")
  }

  def Value: Parser[Term] = {
    "true" ^^^ True |
      "false" ^^^ False |
      //"0" ^^^ Zero() |
      NumericValue |
      failure("illegal start of expression")
  }

  def NumericValue: Parser[Term] = {
    numericLit ^^ { case e => Numeric(e.toInt) } |
      "succ" ~> NumericValue ^^ { case e => Succ(e) } |
      failure("illegal start of expression")
  }

  val noRuleException = new Exception("No rules applied");

  def isnumericval(t: Term): Boolean = {
    t match {
      case Zero => true
      case Succ(t) => isnumericval(t)
      case _ => false
    }
  }

  def isval(t: Term): Boolean = {
    t match {
      case True => true
      case False => true
      case e if (isnumericval(e) == true) => true
      case _ => false
    }
  }

  def printStuckTerm(t: Term) = println("Stuck term: " + t)

  def reduce(t: Term): Term = {
    t match {
      case If(True, t2, t3) => t2
      case If(False, t2, t3) => t3
      case If(t1, t2, t3) => val v = reduce(t1); If(v, t2, t3)
      case IsZero(Zero) => True
      case IsZero(Succ(tm)) if (isnumericval(tm) == true) => False
      case Pred(Zero) => Zero
      case Pred(Succ(tm)) if (isnumericval(tm) == true) => tm
      case IsZero(tm) => val v = reduce(tm); IsZero(v)
      case Pred(tm) => val v = reduce(tm); Pred(v)
      case Succ(tm) => val v = reduce(tm); Succ(v)
      case Numeric(0) => Zero
      case Numeric(tm) if (tm != 0) => Succ(reduce(Numeric(tm - 1)))
      case _ => throw NoRuleException(t.toString);
    }
  }

  def eval(t: Term): Term = {
    //    println("term :"+ t)
    t match {
      case tm if (isval(tm)) => tm
      case Numeric(0) => Zero
      case Numeric(tm) if (tm != 0) => Succ(eval(Numeric(tm - 1)))
      case If(t1, t2, t3) if (eval(t1) == True && isval(eval(t2))) => eval(t2)
      case If(t1, t2, t3) if (eval(t1) == False && isval(eval(t3))) => eval(t3)
      case Succ(t1) if (isnumericval(eval(t1))) => Succ(eval(t1))
      case Pred(t1) => eval(t1) match {
        case Zero => Zero
        case Succ(tm) if (isnumericval(tm)) => tm
        case _ => throw NoRuleException(t.toString)
      }
      case IsZero(t1) => eval(t1) match {
        case Zero => True;
        case Succ(tm) if (isnumericval(tm)) => False
        case _ => throw NoRuleException(t.toString);
      }
      case _ => throw NoRuleException(t.toString)
    }
  }

  //small-step-reduction
  def eval_small_steps(t: Term): Unit = {
    println(t)
    if (!isval(t)) {
      try {
        eval_small_steps(reduce(t))
      } catch {
        case NoRuleException(_) => println("Stuck term: " + t)
      }
    }
  }

  def eval_big_steps(t: Term): Unit = {
    print("Big steps : ")
    try {
      print(eval(t))
    } catch {
      case NoRuleException(s) => println("Stuck term: " + s)
    }
  }

  def main(args: Array[String]): Unit = {
    //val tokens = new lexical.Scanner(StreamReader(new java.io.InputStreamReader(System.in)))
    val intput = "succ if iszero 0 then true else false"
    val tokens = new lexical.Scanner(intput)
    phrase(Expr)(tokens) match {
      case Success(trees, _) =>
        eval_small_steps(trees)
        eval_big_steps(trees)
      case e =>
        println(e)
    }
  }
}
