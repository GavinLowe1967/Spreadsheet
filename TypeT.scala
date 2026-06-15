package spreadsheet

import TypeVar.TypeID
import TypeParam.{TypeParamName, TypeParamMap}

/** The trait representing all type expressions. */
trait TypeT{
  def asString: String

  /** The type parameters included in this type. */
  def typeParams: List[TypeParamName]

  /** The TypeVars in this type. */
  def typeVars: List[TypeVar]

  /** Rename newly bound type parameters that name-clash with a member of tps to
    * fresh values. */
  def renameTypeParams(tps: Set[TypeParamName]): TypeT = 
    renameTypeParams(TypeParam.newTypeParamMap, tps)

  /** Rename the type parameters according to f.  Also rename newly bound type
    * parameters that name-clash with a member of tps to fresh values.  Here
    * dom f is a subset of tps. */
  def renameTypeParams(f: TypeParamMap, tps: Set[TypeParamName]): TypeT

  /** Does this contain a FunctionValue whose return type has not yet been
    * set? */
  def hasNullReturnFunction: Boolean
}

object TypeT{
  /** Types of values in cells. */
  val CellTypes = List(IntType, FloatType, StringType, BoolType)

  /** Numeric types. */
  val NumTypes = List(IntType, FloatType) 

  /** String representing ts. */
  def showList(ts: List[TypeT]) =
    if(ts.isEmpty) "()" else ts.map(_.asString).mkString(", ")
}

// ==================================================================

/** A type variable.  Used for the types of Lists and to replace formal
  * parameters of a function when the function is applied. */
case class TypeVar(tv: TypeID) extends TypeT{
  def asString = s"t$tv"                   
  def typeParams = List()
  def typeVars = List(this)
  def renameTypeParams(f: TypeParamMap, tps: Set[TypeParamName]) = this
  def hasNullReturnFunction = false
}


object TypeVar{
  /** The type of identifiers for type variables. */
  type TypeID = Int

  /** The next identifier to use for a type variable. */
  private var next = 0

  def nextTypeID(): TypeID = { next += 1; next-1 }
}

// ================================================================

/** The type for an untyped cell expression. */
case class CellTypeVar(tv: TypeID) extends TypeT{
  def asString = s"t$tv"                   
  def typeParams = List()
  def typeVars = List()
  def renameTypeParams(f: TypeParamMap, tps: Set[TypeParamName]) = this
  def hasNullReturnFunction = false
}

// ==================================================================

/** Marker trait for types that can be associated with a cell read such as
  * "#A3: <type>" -- CellTypes and TypeParams. */
trait CellReadType extends TypeT

/** A type parameter, named in the script. */
case class TypeParam(name: String) extends CellReadType{
  def asString = name 
  def typeParams = List(name)
  def typeVars = List()
  def renameTypeParams(f: TypeParamMap, tps: Set[TypeParamName]) =
    f.get(name) match{
      case Some(n1) => TypeParam(n1); case None => this 
    }
  def hasNullReturnFunction = false
}

object TypeParam{
  type TypeParamName = String

  import scala.collection.immutable.Map

  /** Maps to rename TypeParams, for use in renameTypeParams. */
  type TypeParamMap = Map[TypeParamName, TypeParamName]

  val newTypeParamMap = Map[TypeParamName,TypeParamName]()

  /** Map to control production of new names for type parameters.  If
    * `newNameMap(name) = n` then the most recent fresh name for `name` was
    * `name`#`n`. */
  private val newNameMap = 
    new scala.collection.mutable.HashMap[TypeParamName, Int]

  /** Get a fresh name corresponding to `name`. */
  def getNewName(name: TypeParamName): TypeParamName = {
    val n = newNameMap.get(name) match{ case Some(m) => m+1; case None => 0 }
    newNameMap += name -> n
    s"$name#$n"
  }
}

// ==================================================================

/** A marker trait for atomic equality types. */
trait EqType extends TypeT

/** A marker trait for types with order relations. */
trait OrdType extends EqType 
// Note: at present, only implementing types are CellType.  This should be
// extended.

/** Marker trait for base types, i.e. atomic. */
trait BaseType extends TypeT{
  def typeParams = List()
  def typeVars = List()
  def renameTypeParams(f: TypeParamMap, tps: Set[TypeParamName]) = this
  def hasNullReturnFunction = false
}

/** A marker trait for types that can appear in cells of the spreadsheet.
  * Note: this includes ErrorType and EmptyType. */
trait CellValueType extends OrdType with BaseType

/** A marker trait for types that are subclasses of the script type
  * `CellType`. */
trait CellType extends CellValueType with CellReadType

/** A marker trait for types that are subclasses of the script type
  * `NumType`. */
trait NumType extends CellType

/* Now all the base types. */

case object IntType extends NumType {
  def asString = "Int"
}

case object FloatType extends NumType{
  def asString = "Float"
}

case object BoolType extends CellType{
  def asString = "Boolean"
}

case object StringType extends CellType{
  def asString = "String"
}

case object UnitType extends OrdType with BaseType{
  def asString = "Unit"
}

case object RowType extends OrdType with BaseType{
  def asString = "Row"
}

case object ColumnType extends OrdType with BaseType{
  def asString = "Column"
}

case object EmptyType extends CellValueType{
  def asString = "empty cell"
}

case object ErrorType extends CellValueType{
  def asString = "error value"
}

// ==================================================================

/** The type of lists with underlying type `underlying`. */
case class ListType(underlying: TypeT) extends TypeT{
  def asString = { val u = underlying.asString; s"List[$u]" }

  def typeParams = underlying.typeParams

  def typeVars = underlying.typeVars

  def renameTypeParams(f: TypeParamMap, tps: Set[TypeParamName]) = 
    ListType(underlying.renameTypeParams(f, tps))

  def hasNullReturnFunction = underlying.hasNullReturnFunction
}

// ==================================================================

case class TupleType(componentTs: List[TypeT]) extends TypeT{
  val arity = componentTs.length
  require(2 <= arity && arity <= TupleType.MaxArity)

  def asString = componentTs.map(_.asString).mkString("(", ",", ")")

  def typeParams = componentTs.flatMap(_.typeParams).distinct

  def typeVars = componentTs.flatMap(_.typeVars).distinct

  def renameTypeParams(f: TypeParamMap, tps: Set[TypeParamName]) = 
    TupleType(componentTs.map(_.renameTypeParams(f, tps)))

  def hasNullReturnFunction = componentTs.exists(_.hasNullReturnFunction)
}

object TupleType{
  val MaxArity = 4 // 12

  def apply(componentTs: TypeT*) = new TupleType(componentTs.toList)
  // Note: test.dir should include a test for the get function of maximum arity.
}


// ==================================================================

// ====== Note: FunctionType is in its own file, as it depends on TypeConstraint
