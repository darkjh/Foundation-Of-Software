package fos

import scala.collection.mutable.{ Map, HashMap };

case class TypeError(msg: String) extends Exception(msg)

object Type {

  import CT._
  import Utils._

  type Class = String
  type Context = List[Pair[Class, String]]

  //def typeOf(tree: Tree, ctx: Context): Class = 
  //   ... To complete ... 
}

case class EvaluationException(msg: String) extends Exception
case class NoRuleApplies(expr: Expr) extends Exception

object Evaluate extends (Expr => Expr) {

  import Utils._

  def apply(expr: Expr): Expr = {
    //    println("expr = " + expr)
    expr match {
      // Computation rules:

      // R-FIELD
      case Select(New(cls, fds), fd) =>
        val clsdef = getClassDef(cls)
        clsdef.findField(fd) match {
          case None => throw new EvaluationException("Field not found")
          case Some(field) => fds(clsdef.fields.indexOf(field))
        }
      // R-INVK
      case Apply(newClass @ New(cls, fds), mtd, args) =>
        getClassDef(cls).findMethod(mtd) match {
          case None => throw new EvaluationException("Method not found")
          case Some(method) => substituteInBody(method.body, newClass, method.args zip args)
        }
      // R-CAST
      case Cast(castClass, castCls @ New(cls, fds)) if getClassDef(cls).isSubClassOf(castClass) => castCls

      // Congruence rules

      // RC-FIELD
      case Select(e, f) => Select(Evaluate(e), f)
      // RC-INVK-RECV
      case Apply(e, mtd, args) if !isValue(e) => Apply(Evaluate(e), mtd, args)
      // RC-INVK-ARG
      case Apply(e, mtd, args) if !args.forall(isValue(_)) =>
        Apply(e, mtd, args map { case exp => if (isValue(exp)) exp else Evaluate(exp) })
      // RC-NEW-ARG
      case New(cls, args) if !args.forall(isValue(_)) =>
        New(cls, args map { case exp => if (isValue(exp)) exp else Evaluate(exp) })
      // RC-CAST
      case Cast(cls, e) => Cast(cls, Evaluate(e))

      // No rules (Value)
      case _ => throw new NoRuleApplies(expr)
    }
  }

  def isValue(expr: Expr): Boolean = expr match {
    case New(cls, Nil) => true
    case New(cls, fds) => fds.forall(exp => isValue(exp))
    case _ => false
  }

  def substituteInBody(exp: Expr, thiss: New, substs: List[(FieldDef, Expr)]): Expr = exp match {
    case Select(obj: Expr, field: String) => Select(substituteInBody(obj, thiss, substs), field)
    case New(cls, args) => New(cls, args map (arg => substituteInBody(arg, thiss, substs)))
    case Cast(cls, e) => Cast(cls, substituteInBody(e, thiss, substs))
    case Var("this") => thiss
    case Var(bd) => substs find (subs => subs._1.name == bd) match {
      case None => exp
      case Some((_, sub)) => sub
    }

    case Apply(obj, method, args) => Apply(substituteInBody(obj, thiss, substs), method, args map (arg => substituteInBody(arg, thiss, substs)))
    case _ => throw new EvaluationException("Apply: Forgot expression " + exp)
  }
}

object CT {

  val objectClass: String = "Object"
  private val objectClassDef = ClassDef(objectClass, null, Nil, CtrDef(objectClass, Nil, Nil, Nil), Nil)

  private var ct: Map[String, ClassDef] = new HashMap[String, ClassDef]

  add(objectClass, objectClassDef)

  def elements = ct iterator

  def lookup(classname: String): Option[ClassDef] = if (classname != null) ct get classname else None

  def add(key: String, element: ClassDef): Unit = ct += key -> element

  def delete(key: String) = ct -= key

  def clear(): Unit = {
    ct clear;
    add(objectClass, objectClassDef)
  }

}

object Utils {

  def getClassDef(className: String): ClassDef = CT lookup className match {
    case None => throw new TypeError("class " + className + " not declared")
    case Some(c: ClassDef) => c
  }
}
