package spreadsheet

import TypeVar.TypeID
import TypeParam.TypeParamName

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

  /** The type parameters included in this type. */
  def typeParams: List[TypeParamName]

//  def isEqType: Boolean
}

object TypeT{
  val CellTypes = List(IntType, FloatType, StringType, BoolType)
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

  /** Get a fresh TypeVar. */
  // def get: TypeVar = { next += 1; TypeVar(next-1) }
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

// ==================================================================

/** The type of lists with underlying type `underlying`. */
case class ListType(underlying: TypeT) extends TypeT{

  def asString = { val u = underlying.asString; s"List[$u]" }

  override def isSubclassOf(t: TypeT) = t match{
    case AnyType => true
    case ListType(u) => underlying.isSubclassOf(u)
    case other => false
  }

  def typeVars = underlying.typeVars
  def typeParams = underlying.typeParams
}

// ==================================================================



// ====== Note: FunctionType is in its own file, as it depends on TypeConstraint

// /** The type of functions from `domain` to `range`. 
//   * @param params A list of free type identities, paired with a constraint upon
//   * them. */
// case class FunctionType(
//   params: List[FunctionType.TypeParameter], domain: List[TypeT], range: TypeT
// ) extends TypeT{
//   def asString = 
//     domain.map(_.asString).mkString("(", ",", ")")+" => "+range.asString

//   def typeVars = domain.flatMap(_.typeVars) ++ range.typeVars

//   def typeParams = {
//     assert(params.isEmpty)    // ???
//     domain.flatMap(_.typeParams) ++ range.typeParams
//   }
// }

// object FunctionType{
//   type TypeParameter = (TypeParam.TypeParamName, StoredTypeConstraint)
// }

// ==================================================================

/** A polymorphic function.
  * @param params A list of free type identities, paired with a constraint upon
  * them.
  * @param template A template from which to construct the function, by
  * substituting the free type identities with fresh ones.  */
// case class PolymorphicFunction(
//   params: List[(TypeID, StoredTypeConstraint)], template: FunctionType)
//     extends TypeT{

//   def asString = ???

//   def typeVars = ???
// }

// object PolymorphicFunction{
//   val head = {
//     val tid = TypeVar.getNext(); val t = TypeVar(tid)
//     FunctionType(List((tid, AnyTypeConstraint)), List(ListType(t)), t)
//   }
//   val tail = {
//     val tid = TypeVar.getNext(); val t = TypeVar(tid)
//     FunctionType(List((tid, AnyTypeConstraint)), List(ListType(t)), ListType(t))
//   }

    

// }
