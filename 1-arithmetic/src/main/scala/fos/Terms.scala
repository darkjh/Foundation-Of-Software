package fos

import scala.util.parsing.input.Positional

/** Abstract Syntax Trees for terms. */
abstract class Term extends Positional 

//   ... To complete ... 
case object True extends Term

case object False extends Term 

case class Numeric(t: Int) extends Term {
  override def toString =  t.toString
}

case object Zero extends Term 

case class IsZero(t: Term) extends Term 

case class Succ(t: Term) extends Term 

case class Pred(t: Term) extends Term 

case class If(t1: Term, t2: Term, t3: Term) extends Term 

