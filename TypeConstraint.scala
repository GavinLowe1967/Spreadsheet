package spreadsheet

/** The interface of TypeEnv as seen by a TypeConstraint, namely the ability
  * to find the TypeConstraint stored against a TypeVar or TypeParam.
  * Note: this is used to avoid cyclic compilation dependencies. */
trait TypeEnv0{
  /** The constraint associated with TypeVar(tid). */
  def apply(tid: TypeVar.TypeID) : TypeConstraint

  /** The constraint associated with TypeParam(tp). */
  def constraintForTypeParam(tp: TypeParam.TypeParamName) : TypeParamConstraint
}

// ==================================================================

/** A constraint upon a type variable. */
trait TypeConstraint{
  /** Is this constraint satisfied by type t?  */
  def satisfiedBy(typeEnv: TypeEnv0, t: TypeT) : Boolean

  /** The TypeConstraint representing the intersection (or conjunction) of this
    * and other (used in Unification.scala). */
  def intersection(other: TypeConstraint): TypeConstraint

  /** String to use in error messages when this type is expected. */
  def asStringE: String
}

// ==================================================================

/** A type constraint corresponding to a type parameter of a function, either
  * EqTypeConstraint or AnyTypeConstraint.  Note: this represents a universal
  * quantification over the relevant types, and always contains at least two
  * possible types. */
trait TypeParamConstraint extends TypeConstraint{

  /** Does this imply other?  I.e., the types that satisfy this are a subset of
    * the types that satisfy other?  (Used in TypeEnv.scala.) */
  def implies(other: TypeConstraint) =  this.intersection(other) == this

  /** String to use in error messages when this constraint is found.
    * Overwritten in NumTypeConstraint and EqTypeConstraint.*/
  def asString: String = asStringE
}

// ==================================================================

/** A type constraint representing a single type t.  Note: these are stored
  * when the value of a type variable is completely decided, but can't be
  * associated with type parameters of functions. */
case class SingletonTypeConstraint(t: TypeT) extends TypeConstraint{
  def satisfiedBy(typeEnv: TypeEnv0, t1: TypeT) = t1 == t

  def intersection(other: TypeConstraint) = other match{
    case AnyTypeConstraint => this
    case _ =>  println(s"$t $other"); ??? //FIXME
  }

  def asStringE = t.asString
}

// ==================================================================


/** The type constraint corresponding to being an equality type. */
case object EqTypeConstraint extends TypeParamConstraint{
  def satisfiedBy(typeEnv: TypeEnv0, t: TypeT) = {
    // Note: t might be a typeVar in a recursive call for ListType(t)
    t match{
      case _: EqType => true
      case ListType(underlying) => satisfiedBy(typeEnv, underlying)
      case TypeVar(tid) => typeEnv(tid) match{
        //case EqTypeConstraint | OrdTypeConstraint => true
        case SingletonTypeConstraint(t) => // can this happen?
          println(s"EqTypeConstraint $t"); satisfiedBy(typeEnv, t)
        case tpc: TypeParamConstraint => tpc.implies(this)
        //case AnyTypeConstraint => false
      }
      case TypeParam(tp) => typeEnv.constraintForTypeParam(tp).implies(this)
      case _ => false // FunctionType, TupleType *IMPROVE*
    }
  }

  def intersection(other: TypeConstraint) = other match{
    case OrdTypeConstraint => OrdTypeConstraint
    case EqTypeConstraint | AnyTypeConstraint => EqTypeConstraint
      // tested by  applyE(threeE, true) in TypeCheckerTest2
    case SingletonTypeConstraint(_) => ???
  }

  override def asString = "Eq" 

  def asStringE = "equality type"
}

// =================================================================

case object OrdTypeConstraint extends TypeParamConstraint{
  def satisfiedBy(typeEnv: TypeEnv0, t: TypeT) = {
    // Note: t might be a typeVar in a recursive call for ListType(t)
    t match{
      case _: OrdType => true
      case ListType(underlying) => satisfiedBy(typeEnv, underlying)
      case TypeVar(tid) => typeEnv(tid) match{
        // case OrdTypeConstraint => true
        case SingletonTypeConstraint(t) => // can this happen?
          println(s"OrdTypeConstraint $t"); satisfiedBy(typeEnv, t)
        case tpc: TypeParamConstraint => tpc.implies(this)
        //case EqTypeConstraint | AnyTypeConstraint => false
      }
      case TypeParam(tp) => typeEnv.constraintForTypeParam(tp).implies(this)
      case _ => false // FunctionType, TupleType
    }
  }

  def intersection(other: TypeConstraint) = other match{
    case EqTypeConstraint | OrdTypeConstraint | AnyTypeConstraint => 
      OrdTypeConstraint
    case SingletonTypeConstraint(_) => ???
  }

  override def asString = "Ord" 

  def asStringE = "Ord type"
}

// ==================================================================

/** The trivial type constraint, that allows all types. */
case object AnyTypeConstraint extends TypeParamConstraint{
  def satisfiedBy(typeEnv: TypeEnv0, t: TypeT) = true

  def intersection(other: TypeConstraint) = other

  def asStringE = "any type" // never used? 
}
