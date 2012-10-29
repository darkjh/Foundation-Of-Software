package fos

import scala.util.parsing.input.Positional

/** Abstract Syntax Trees for terms. */
abstract class Term extends Positional

case object True extends Term {
  override def toString() = "true"
}

case object False extends Term {
  override def toString() = "false"
}

case object Zero extends Term {
  override def toString() = "0"
}

case class IsZero(t: Term) extends Term 

case class Succ(t: Term) extends Term 

case class Pred(t: Term) extends Term 

//case class Numeric(t: Int) extends Term {
//  override def toString =  t.toString
//}

case class If(t1: Term, t2: Term, t3: Term) extends Term 

case class Var(x: String) extends Term {
  val v = x
  override def toString = x
}

case class Abs(x: Var, tp: Type, t: Term) extends Term {
  override def toString = "(" + "\\" + x + ":" + tp + "." + t + ")" 
}

case class App(left: Term, right: Term) extends Term {
  override def toString = (left, right) match {
    case (x: Term, y: App) => left + " " + "(" + right + ")"
    case (x: Term, y: Term) => left + " " + right
  }
}

case class Pair(fst: Term, snd: Term) extends Term {
  override def toString = "{" + fst + "," + snd + "}"
}

case class Fst(p: Term) extends Term
case class Snd(p: Term) extends Term

/** Abstract Syntax Trees for types. */
abstract class Type extends Term

case object TypeBool extends Type {
  override def toString() = "Bool"
}

case object TypeNat extends Type {
  override def toString() = "Nat"
}

case class TypeFun(from: Type, to: Type) extends Type {
  override def toString = (from, to) match {
    case (x: TypeFun, y: TypeFun) => "(" + from + ")" + "->" + "(" + to + ")"
    case (x: TypeFun, y: Type) => "(" + from + ")" + "->" + to
    case (x: Type, y: Type) => from + "->" + to
  }
}

case class TypePair(fst: Type, snd: Type) extends Type {
  val f = fst
  val s = snd
  override def toString = (f, s) match {
    case (x: TypePair, y: TypePair) => "(" + f + ")" + "*" + "(" + s + ")"
    case (x: TypePair, y: Type) => "(" + f + ")" + "*" + s
    case (x: Type, y: Type) => f + "*" + s
  }
}
