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

case class If(t1: Term, t2: Term, t3: Term) extends Term 

case class Var(x: String) extends Term {
  val v = x
  override def toString = x
}

case class Abs(x: Var, tp: Type, t: Term) extends Term {
  override def toString = "(" + "\\" + x + ":" + tp + "." + t + ")" 
}

case class App(left: Term, right: Term) extends Term {
  override def toString =
    left + " " + (right match {
      case _: App => "(" + right + ")"
      case _      => right
    })
}

case class Pair(fst: Term, snd: Term) extends Term {
  override def toString = "{" + fst + "," + snd + "}"
}

case class Fst(p: Term) extends Term
case class Snd(p: Term) extends Term

case class Inl(p: Term, tp: Type) extends Term
case class Inr(p: Term, tp: Type) extends Term
case class Case(sum: Term, inl: String, t1: Term, inr: String, t2: Term) extends Term



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
  override def toString = (fst, snd) match {
    case (x: TypePair, y: TypePair) => "(" + fst + ")" + "*" + "(" + snd + ")"
    case (x: TypePair, y: Type) => "(" + fst + ")" + "*" + snd
    case (x: Type, y: Type) => fst + "*" + snd
  }
}

case class TypeSum(inl: Type, inr: Type) extends Type {
  override def toString = (inl, inr) match {
    case (x: TypeSum, y: TypeSum) => "(" + inl + ")" + "+" + "(" + inr + ")"
    case (x: TypeSum, y: Type) => "(" + inl + ")" + "+" + inr
    case (x: Type, y: Type) => inl + "+" + inr
  }
}

