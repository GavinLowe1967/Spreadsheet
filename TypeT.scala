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

/** A type variable.  Used for the types of Lists and to replace formal
  * parameters of a function when the function is applied. */
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
  def asString = name 
  def typeVars = List()
  def typeParams = List(name)
}

object TypeParam{
  type TypeParamName = String
}

// ==================================================================

/** A marker trait for atomic equality types. */
trait EqType extends TypeT

/** Marker trait for base types, i.e. atomic. */
trait BaseType extends TypeT{
  def typeVars = List()
  def typeParams = List()
}

/** A marker trait for types that can appear in cells of the spreadsheet. */
trait CellType extends EqType with BaseType

/* Now all the base types. */

case object IntType extends CellType {
  def asString = "Int"
}

case object FloatType extends CellType{
  def asString = "Float"
}

case object BoolType extends CellType{
  def asString = "Boolean"
}

case object StringType extends CellType{
  def asString = "String"
}

case object RowType extends EqType with BaseType{
  def asString = "Row"
}

case object ColumnType extends EqType with BaseType{
  def asString = "Column"
}

case object EmptyType extends CellType{
  def asString = "empty cell"
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
