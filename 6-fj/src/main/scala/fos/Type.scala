package fos

import scala.collection.mutable.{ Map, HashMap };
import scala.util.parsing.input.Position

case class TypeError(pos: Position, msg: String) extends Exception(msg) {
  override def toString =
      "Type Error: " + msg + "\n" + pos.longString
}

object Type {

  import CT._
  import Utils._

  type Class = String
  type Context = List[Pair[Class, String]]
  
  def typeOf(tree: Tree, ctx: Context): Class = tree match {
    /* T - Var */
    case vv @ Var(name) => ctx find (_._2 == name) match {
      case None => throw new TypeError(tree.pos, "variable definition not found")
      case Some(v) => v._1 
    }
    
    /* T - Field */
    case ss @ Select(obj, field) => typeOf(obj, ctx) match {
      case c: Class => 
        getClassDef(c).findField(field) match {
          case None => throw new TypeError(tree.pos, "class field not found")
          case Some(v) => v.tpe
        }
      case _ => throw new TypeError(tree.pos, "")
    }
    
    /* T - Invk */
    case aa @ Apply(obj, method, args) => typeOf(obj, ctx) match {
      case c: Class => 
        val argsType = args map (typeOf(_, ctx))
        val m = getClassDef(c).findMethod(method) match {
          case None => throw new TypeError(tree.pos, "method not defined")
          case Some(v) => v
        }
        try {
          m.checkTypeArguments(argsType)
        } catch {
          case MethodArgsException(msg) => throw new TypeError(tree.pos, msg)
        }
        m.tpe
      case _ => throw new TypeError(tree.pos, "")
    }
    
    /* T - New */
    case nn @ New(cls, args) => {
      val argsType = args map (typeOf(_, ctx))
      val classdef = getClassDef(cls)
      try {
    	classdef.checkTypeArguments(argsType)
      } catch {
        case ClassConstructorArgsException(msg) => throw new TypeError(tree.pos, msg)
      }
      classdef.name
    }
    
    /* T - UCast, T - DCast, T - SCast */
    case cc @ Cast(cls, e) => typeOf(e, ctx) match {
      case c: Class => 
        val fromClass = getClassDef(c)
        val toClass = getClassDef(cls)
        if (fromClass.isSubClassOf(toClass)) toClass.name
        else if (toClass.isSubClassOf(fromClass) && !toClass.name.equals(fromClass.name)) toClass.name 
        else { // stupid cast
          System.err.println("stupid cast found\n" + tree.pos.longString)
          toClass.name
        }
      case _ => throw new TypeError(tree.pos, "")
    }
    
    /* Class Typing */
    case cc @ ClassDef(name, scls, fields, ctor, methods) => {
      try {
        cc.checkFields
        cc.verifyConstructorArgs
      } catch {
        case FieldAlreadyDefined(msg) => throw new TypeError(tree.pos, msg)
        case ClassConstructorArgsException(msg) => throw new TypeError(tree.pos, msg)
      }
      
      val superParams = ctor.supers map (_.name)
      val superFields = getClassDef(scls).fields map (_.name)
      val localFields = fields map (_.name)
      
      // syntax errors, should be handled in parser
      if (!name.equals(ctor.name))
        throw new TypeError(tree.pos, "ctor name mismatch")
      if (!(ctor.body map (_.obj) forall (_.equals("this"))))
        throw new TypeError(tree.pos, "this expected")
      
      // super class fields should be all init
      if (superParams.length != superFields.length)
        throw new TypeError(tree.pos, "super class fields are not properly initialized")
      else if (!((superParams zip superFields) forall (p => p._1.equals(p._2))))
        throw new TypeError(tree.pos, "super class fields are not properly initialized")
      
      // local fields should be all init
      if (ctor.body.length != localFields.length)
        throw new TypeError(tree.pos, "class fields are not properly initialized")
      else if (!((ctor.body zip localFields) forall (p => p._1.field.equals(p._2))))
        throw new TypeError(tree.pos, "class fields are not properly initialized")
      
      // rhs should be typeble
      val ctorCtx = (ctor.args map (_.tpe)) zip (ctor.args map (_.name)) 
      (ctor.body map (_.rhs)) map (typeOf(_, ctorCtx))
            
      /* Method Typing */
      for (m <- methods) {
        val localCtx = (name, "this") :: ((m.args map (_.tpe)) zip (m.args map (_.name)))
        val clsMethod = typeOf(m.body, localCtx)
        getClassDef(clsMethod).isSubClassOf(m.tpe)
        
        // check override methods, if any
        try {
          cc.overrideMethod(m.tpe, m.name, m.args, m.body)
        } catch {
          case MethodOverrideException(msg) => 
            throw new TypeError(tree.pos, msg)
        }
      }
      name
    }
    case _ => throw new TypeError(tree.pos, "unknown structure")
  }
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
          case Some(field) => fds((clsdef.getFieldsSuperclass ::: clsdef.fields).indexOf(field))
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
      case Select(e, f) if !isValue(e) => Select(Evaluate(e), f)
      // RC-INVK-RECV
      case Apply(e, mtd, args) if !isValue(e) => Apply(Evaluate(e), mtd, args)
      // RC-INVK-ARG
      case Apply(e, mtd, args) if !args.forall(isValue(_)) =>
        Apply(e, mtd, args map { case exp => if (isValue(exp)) exp else Evaluate(exp) })
      // RC-NEW-ARG
      case New(cls, args) if !args.forall(isValue(_)) =>
        New(cls, args map { case exp => if (isValue(exp)) exp else Evaluate(exp) })
      // RC-CAST
      case Cast(cls, e) if !isValue(e) => Cast(cls, Evaluate(e))

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
    case None => throw new Exception("class " + className + " not declared")
    case Some(c: ClassDef) => c
  }
}
