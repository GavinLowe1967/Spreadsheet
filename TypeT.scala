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

  /** The type variables included in this type. */
  def typeVars: List[TypeID]
}

// ==================================================================

/** A type variable. */
case class TypeVar(tv: TypeID) extends TypeT{
  def asString = toString

  def typeVars = List(tv)
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

case object AnyType extends TypeT{
  def asString = "Any"

  def typeVars = List()
}

// ==================================================================

case object IntType extends TypeT{
  def asString = "Int"
  def typeVars = List()
}

case object BoolType extends TypeT{
  def asString = "Boolean"
  def typeVars = List()
}

case object EmptyType extends TypeT{
  def asString = "Empty"
  def typeVars = List()
}
// Note: this type shouldn't appear in a function declaration. 

case object StringType extends TypeT{
  def asString = "String"
  def typeVars = List()
}

case object RowType extends TypeT{
  def asString = "Row"
  def typeVars = List()
}

case object ColumnType extends TypeT{
  def asString = "Column"
  def typeVars = List()
}

/** The type of lists with underlying type `underlying`. */
case class ListType(underlying: TypeT) extends TypeT{
  def asString = s"List[${underlying.asString}]"

  override def isSubclassOf(t: TypeT) = t match{
    case AnyType => true
    case ListType(u) => underlying.isSubclassOf(u)
    case other => false
  }

  def typeVars = underlying.typeVars
}

/** The type of functions from `domain` to `range`. */
case class FunctionType(domain: List[TypeT], range: TypeT) extends TypeT{
  def asString = 
    domain.map(_.asString).mkString("(", ",", ")")+" => "+range.asString

  // FIXME: override isSubclassOf?

  def typeVars = domain.flatMap(_.typeVars) ++ range.typeVars
}
