package fos

import scala.collection.immutable.{ Set, ListSet }
import com.sun.org.apache.xerces.internal.impl.xs.SubstitutionGroupHandler
import scala.collection.mutable.HashMap

abstract class Type {
  override def toString() = this match {
    case TypeVar(a) => a
    case TypeFun(a, b) => "(" + a + " -> " + b + ")"
    case TypeNat => "Nat"
    case TypeBool => "Bool"
  }
}

case class TypeVar(name: String) extends Type
case class TypeFun(t1: Type, t2: Type) extends Type
case object TypeNat extends Type
case object TypeBool extends Type

/** Type Schemes are not types. */
case class TypeScheme(args: List[TypeVar], tp: Type) {
  override def toString() = args.mkString("[", ", ", "].") + tp
  
//  def instantiate(): Type = {
//    var subst: Substitution = emptySubst
//    for (a <- args) subst = subst.extend(a, TypeVar(Type.freshTypeName(a.name)))
//    subst(tp)
//  }
  var countMap = args map (p => p.name) zip (args map (p => 0)) toMap
  def instantiate: Type = tp match {
    case tvar @ TypeVar(str) =>
      if (args.exists(_.name.equals(str))) {
        val currentVal = countMap(str)
        countMap = countMap.updated(str, currentVal + 1)
        TypeVar(str.toUpperCase + currentVal)
      } else
        tp
    case TypeFun(a, b) => TypeFun(TypeScheme(args, a).instantiate, TypeScheme(args, b).instantiate)
    case t => t
  }
}

// type related static methods
object Type {
  
  var index = 0
  def freshTypeName: String = { index = index + 1; "a" + index }
  
//  val index: HashMap[String, Int] = new HashMap()
//  var baseIndex = 64.toChar
//  
//  def freshTypeName(base: String): String = {
//    if (!index.contains(base)) {
//      index += ((base, 0))
//      base + 0
//    } else {
//      val i = index(base) + 1
//      index.update(base, i)
//      base + i
//    }
//  }
//  
//  def freshTypeBase(): String = {
//    baseIndex = (baseIndex.toInt + 1).toChar
//    baseIndex.toString
//  }
}

abstract class Substitution extends (Type => Type) { 
  subst => 
  
  var indent = 0
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
