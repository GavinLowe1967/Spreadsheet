package spreadsheet

import TypeVar.TypeID

/** The trait representing all type expressions. */
trait TypeT{
  def asString: String

  /** Is this a subclass of t? 
    * Note: this is overwritten in some subclasses. */
  def isSubclassOf(t: TypeT) = t == AnyType || t == this

  def comparable(t: TypeT) = 
    t.isSubclassOf(this) || this.isSubclassOf(t)
// TODO: do we need the above two? 

  /** The type variables included in this type. */
  def typeVars: List[TypeID]

  def isEqType: Boolean
}

// ==================================================================

/** A type variable. */
case class TypeVar(tv: TypeID) extends TypeT{
  def asString = toString

  def typeVars = List(tv)

  def isEqType = ???                  // FIXME
}

object TypeVar{
  /** The type of identifiers for type variables. */
  type TypeID = Int

  /** The next identifier to use for a type variable. */
  private var next = 0

  /** Get a fresh TypeVar. */
  def get: TypeVar = { next += 1; TypeVar(next-1) }
}

// ==================================================================

/** A marker trait for atomic equality types. */
trait EqType extends TypeT{
  def isEqType = true
}

case object IntType extends EqType {
  def asString = "Int"
  def typeVars = List()
  //def isEqType = true
}

case object FloatType extends EqType{
  def asString = "Float"
  def typeVars = List()
  //def isEqType = true
}

case object BoolType extends EqType{
  def asString = "Boolean"
  def typeVars = List()
  // def isEqType = true
}

case object StringType extends EqType{
  def asString = "String"
  def typeVars = List()
  // def isEqType = true
}

case object RowType extends EqType{
  def asString = "Row"
  def typeVars = List()
  // def isEqType = true
}

case object ColumnType extends EqType{
  def asString = "Column"
  def typeVars = List()
  // def isEqType = true
}

// ==================================================================

/** The type of lists with underlying type `underlying`. */
case class ListType(underlying: TypeT) extends TypeT{
  def asString = s"List[${underlying.asString}]"

  override def isSubclassOf(t: TypeT) = t match{
    case AnyType => true
    case ListType(u) => underlying.isSubclassOf(u)
    case other => false
  }

  def typeVars = underlying.typeVars

  def isEqType = ???                   // FIXME
}

// ==================================================================

/** The type of functions from `domain` to `range`. */
case class FunctionType(domain: List[TypeT], range: TypeT) extends TypeT{
  def asString = 
    domain.map(_.asString).mkString("(", ",", ")")+" => "+range.asString

  // FIXME: override isSubclassOf?

  def typeVars = domain.flatMap(_.typeVars) ++ range.typeVars

  def isEqType = false
}
