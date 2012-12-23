package fos

import scala.collection.mutable.{Map,HashMap};



case class TypeError(msg: String) extends Exception(msg)

object Type {

  import CT._
  import Utils._
  
  type Class = String
  type Context = List[Pair[Class, String]]
  
  def typeOf(tree: Tree, ctx: Context): Class = tree match {
    /* T - Var */
    case vv @ Var(name) => ctx find (_._2 == name) match {
      case None => throw new TypeError("variable definition not found, at " + vv.pos)
      case Some(v) => v._1 
    }
    
    /* T - Field */
    case ss @ Select(obj, field) => typeOf(obj, ctx) match {
      case c: Class => 
        getClassDef(c).findField(field) match {
          case None => throw new TypeError("class field not found, at " + ss.pos)
          case Some(v) => v.tpe
        }
      case _ => throw new TypeError("")
    }
    
    /* T - Invk */
    case aa @ Apply(obj, method, args) => typeOf(obj, ctx) match {
      case c: Class => 
        val argsType = args map (typeOf(_, ctx))
        val m = getClassDef(c).findMethod(method) match {
          case None => throw new TypeError("method not defined, at " + aa.pos)
          case Some(v) => v
        }
        try {
          m.checkTypeArguments(argsType)
        } catch {
          case MethodArgsException(msg) => throw new TypeError(msg + ", at " + aa.pos)
        }
        m.tpe
      case _ => throw new TypeError("")
    }
    
    /* T - New */
    case nn @ New(cls, args) => {
      val argsType = args map (typeOf(_, ctx))
      val classdef = getClassDef(cls)
      try {
    	classdef.checkTypeArguments(argsType)
      } catch {
        case ClassConstructorArgsException(msg) => throw new TypeError(msg + ", at " + nn.pos)
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
          System.err.println("stupid cast found, at " + cc.pos)
          toClass.name
        }
      case _ => throw new TypeError("")
    }
    
    /* Class Typing */
    case cc @ ClassDef(name, scls, fields, ctor, methods) => {
      try {
        cc.checkFields
        cc.verifyConstructorArgs
      } catch {
        case FieldAlreadyDefined(msg) => throw new TypeError(msg + ", at " + cc.pos)
        case ClassConstructorArgsException(msg) => throw new TypeError(msg + ", at " + cc.pos)
      }
      
      val superParams = ctor.supers map (_.name)
      val superFields = getClassDef(scls).fields map (_.name)
      val localFields = fields map (_.name)
      if (superParams.length != superFields.length)
        throw new TypeError("super class fields are not properly initialized, at " + cc.pos)
      else if (!((superParams zip superFields) forall (p => p._1.equals(p._2))))
        throw new TypeError("super class fields are not properly initialized, at " + cc.pos)
      
      if (ctor.body.length != localFields.length)
        throw new TypeError("class fields are not properly initialized, at " + cc.pos)
      else if (!((ctor.body zip localFields) forall (p => p._1.field.equals(p._2))))
        throw new TypeError("class fields are not properly initialized, at " + cc.pos)
      
      /* Method Typing */
      for (m <- methods) {
        val localCtx = (name, "this") :: ((m.args map (_.tpe)) zip (m.args map (_.name)))
        val clsMethod = typeOf(m.body, localCtx)
        getClassDef(clsMethod).isSubClassOf(m.tpe)
        
        cc.overrideMethod(m.tpe, m.name, m.args, m.body)
      }
      name
    }
    
    case _ => throw new TypeError("unknown structure")
  } 
  
  
    
}

case class EvaluationException(msg: String) extends Exception

object Evaluate extends (Expr => Expr) {
  
  import Utils._
  
  def apply(expr: Expr): Expr = expr
  //   ... To complete ... 

  def substituteInBody(exp: Expr, thiss: New, substs: List[(FieldDef,Expr)]): Expr = exp match {
    case Select(obj: Expr, field: String)  => Select(substituteInBody(obj, thiss, substs),field)
    case New(cls, args)                    => New(cls,args map (arg => substituteInBody(arg, thiss, substs)))
    case Cast(cls, e)                      => Cast(cls,substituteInBody(e, thiss, substs))
    case Var("this")                       => thiss
    case Var(bd) => substs find (subs => subs._1.name == bd) match {
        case None => exp
        case Some((_,sub)) => sub
      }

    case Apply(obj,method,args)            => Apply(substituteInBody(obj, thiss, substs), method, args map (arg => substituteInBody(arg, thiss, substs)))
    case _                                 => throw new EvaluationException("Apply: Forgot expression "+exp)
  }
}

object CT {
  
  val objectClass: String = "Object"
  private val objectClassDef = ClassDef(objectClass, null, Nil, CtrDef(objectClass, Nil, Nil, Nil) , Nil)
  
  private var ct: Map[String, ClassDef] = new HashMap[String, ClassDef]
   
  add(objectClass,objectClassDef)
  
  def elements = ct iterator
  
  def lookup(classname: String): Option[ClassDef] = if(classname != null) ct get classname else None
  
  def add(key: String, element: ClassDef): Unit = ct += key -> element
  
  def delete(key: String) = ct -= key
  
  def clear(): Unit = {
    ct clear;
    add(objectClass,objectClassDef)
  }
  
}


object Utils {
  
  def getClassDef(className: String): ClassDef = CT lookup className match {
    case None => throw new TypeError("class "+className+" not declared")
    case Some(c: ClassDef) => c
  }
}
