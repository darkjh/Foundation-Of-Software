package fos

import scala.collection.immutable.{ Set, ListSet }
import com.sun.org.apache.xerces.internal.impl.xs.SubstitutionGroupHandler

abstract class Type {
  override def toString() = this match {
    case TypeVar(a) => a
    case TypeFun(a, b) => "(" + a + " -> " + b + ")"
    case TypeNat => "Nat"
    case TypeBool => "Bool"
  }
}

case class TypeVar(name: String) extends Type

//   ... To complete ... 
case class TypeFun(t1: Type, t2: Type) extends Type
case object TypeNat extends Type
case object TypeBool extends Type

/** Type Schemes are not types. */
case class TypeScheme(args: List[TypeVar], tp: Type) {
  //   ... To complete ... 
  override def toString() = args.mkString("[", ", ", "].") + tp
}

// type related static methods
object Type {
  var used: List[Int] = List()
  
  def freshTypeName: String = 
    if (used.isEmpty) {
      used = 0 :: used
      "a" + 0
    } else {
      val freshIndex = used.head + 1
      used = freshIndex :: used
      "a" + freshIndex
    }
  
  def tree2type(tr: TypeTree): Type = tr match {
    case NatType => TypeNat
    case BoolType => TypeBool
    case FunType(a, b) => TypeFun(tree2type(a), tree2type(b))
  }
		  
}


abstract class Substitution extends (Type => Type) { 
  subst => 
  
  var indent = 0

  //   ... To complete ...
  def apply(tp: Type): Type = {
    //println("  " * indent + "in: " + tp + "   subst: " + this)
    indent = indent + 1
    val result = tp match {
      case TypeNat => TypeNat
      case TypeBool => TypeBool
      case TypeFun(t1, t2) => TypeFun(this(t1), this(t2))
      case tv: TypeVar => lookup(tv)
    }
    indent = indent - 1
    //println("  " * indent + "out: " + result + "   subst: " + this)
    result
  }
  override def toString() = ""

  def apply(p: (Type, Type)): (Type, Type) = p match {
    case Pair(t1, t2) => (this(t1), this(t2))
  }

  def apply(env: List[(String, TypeScheme)]): List[(String, TypeScheme)] =
    env map { (pair) => (pair._1, TypeScheme(pair._2.args, apply(pair._2.tp))) }

  //   ... To complete ... 
  def lookup(t: TypeVar): Type

  def extend(tv: TypeVar, y: Type) = new Substitution {
    def lookup(t: TypeVar) = {
      if (t == tv) y
      else subst.lookup(t)
    }
  }
  
  def compose(that: Substitution): Substitution = new Substitution {
	  override def apply(tp: Type) = subst(that(tp))
	  def lookup(t: TypeVar): Type = throw new RuntimeException()
  }  
}

/** The empty substitution. */
object emptySubst extends Substitution {
  def lookup(t: TypeVar) = t
}
