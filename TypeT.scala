package spreadsheet

import TypeVar.TypeID
import TypeParam.TypeParamName

/** The trait representing all type expressions. */
trait TypeT{
  def asString: String

  /** The type variables included in this type. */
  def typeVars: List[TypeID]

  /** The type parameters included in this type. */
  def typeParams: List[TypeParamName]
}

object TypeT{
  /** Types of values in cells. */
  val CellTypes = List(IntType, FloatType, StringType, BoolType)

  /** Numeric types. */
  val NumTypes = List(IntType, FloatType) 
}

// ==================================================================

/** A type variable. */
case class TypeVar(tv: TypeID) extends TypeT{
  def asString = toString                                // IMPROVE

  def typeVars = List(tv)

  def typeParams = List()
}

// =========

object TypeVar{
  /** The type of identifiers for type variables. */
  type TypeID = Int

  /** The next identifier to use for a type variable. */
  private var next = 0

  def getNext(): TypeID = { next += 1; next-1 }
}

// ==================================================================

/** A type parameter, named in the script. */
case class TypeParam(name: String) extends TypeT{
  def asString = name // toString

  def typeVars = List()

  def typeParams = List(name)
}

object TypeParam{
  type TypeParamName = String
}

// ==================================================================

/** A marker trait for atomic equality types. */
trait EqType extends TypeT

case object IntType extends EqType {
  def asString = "Int"
  def typeVars = List()
  def typeParams = List()
}

case object FloatType extends EqType{
  def asString = "Float"
  def typeVars = List()
  def typeParams = List()
}

case object BoolType extends EqType{
  def asString = "Boolean"
  def typeVars = List()
  def typeParams = List()
}

case object StringType extends EqType{
  def asString = "String"
  def typeVars = List()
  def typeParams = List()
}

case object RowType extends EqType{
  def asString = "Row"
  def typeVars = List()
  def typeParams = List()
}

case object ColumnType extends EqType{
  def asString = "Column"
  def typeVars = List()
  def typeParams = List()
}

case object EmptyType extends TypeT{
  def asString = "empty cell"
  def typeVars = List()
  def typeParams = List()
}

// ==================================================================

/** The type of lists with underlying type `underlying`. */
case class ListType(underlying: TypeT) extends TypeT{

  def asString = { val u = underlying.asString; s"List[$u]" }

  def typeVars = underlying.typeVars
  def typeParams = underlying.typeParams
}

// ==================================================================



// ====== Note: FunctionType is in its own file, as it depends on TypeConstraint
