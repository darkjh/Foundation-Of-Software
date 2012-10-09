package fos

import scala.util.parsing.input.Positional

/** Abstract Syntax Trees for terms. */
abstract class Term extends Positional

case class Var(x: String) extends Term {
  val v = x
  override def toString = x
}

case class Abs(x: Var, t: Term) extends Term {
  override def toString = "(" + "\\" + x + "." + t + ")" 
}

case class App(left: Term, right: Term) extends Term {
  override def toString = //"(" + left + " " + right + ")"
    (left, right) match {
    case (x: Term, y: App) => left + " " + "(" + right + ")"
    case (x: Term, y: Term) => left + " " + right
  }
}