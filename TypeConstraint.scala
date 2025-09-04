package spreadsheet

/** The interface of TypeEnv as seen by a TypeConstraint, namely the ability
  * to find the TypeConstraint stored against a TypeVar or TypeParam.
  * Note: this is used to avoid cyclic compilation dependencies. */
trait TypeEnv0{
  /** The constraint associated with TypeVar(tid). */
  def apply(tid: TypeVar.TypeID) : StoredTypeConstraint

  /** The constraint associated with TypeParam(tp). */
  def constraintForTypeParam(tp: TypeParam.TypeParamName) : TypeParamConstraint
}

// ==================================================================

/** A constraint upon a type variable. */
trait TypeConstraint

// ==================================================================

/** A type constraint representing a contradiction. */
case object EmptyTypeConstraint extends TypeConstraint
/* Note: the above case can be returned by the intersection operation on a
 * StoredTypeConstraint, but is not itself stored. */ 

// ==================================================================

/** A type constraint that can be stored against a type variable in a type
  * environment.  Any TypeConstraint except EmptyTypeConstraint:
  * SingletonTypeConstraint, MemberOfTypeConstraint, EqTypeConstraint,
  * NumTypeConstraint, AnyTypeConstraint.  */
trait StoredTypeConstraint extends TypeConstraint{
  /** Is this constraint satisfied by type t?  */
  def satisfiedBy(typeEnv: TypeEnv0, t: TypeT) : Boolean

  /** The TypeConstraint representing the intersection (or conjunction) of this
    * and other. */
  def intersection(typeEnv: TypeEnv0, other: StoredTypeConstraint)
      : TypeConstraint

  /** String to use in error messages when this type is expected. */
  def asStringE: String
}

// ==================================================================

/** A type constraint corresponding to a type parameter of a function.  Note:
  * this represents a universal quantification over the relevant types, and
  * always contains at least two possible types. */
trait TypeParamConstraint extends StoredTypeConstraint{

  /** Does this imply other?  I.e., the types that satisfy this are a subset of
    * the types that satisfy other? */
  def implies(other: StoredTypeConstraint): Boolean

  /** String to use in error messages when this constraint is found.
    * Overwritten in NumTypeConstraint and EqTypeConstraint.*/
  def asString: String = asStringE
}

// ==================================================================

/** A type constraint representing a single type t.  Note: these are stored
  * when the value of a type variable is completely decided, but can't be
  * associated with type parameters of functions. */
case class SingletonTypeConstraint(t: TypeT) extends StoredTypeConstraint{
  def satisfiedBy(typeEnv: TypeEnv0, t1: TypeT) = t1 == t

  def intersection(typeEnv: TypeEnv0, other: StoredTypeConstraint) = other match{
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
        case EqTypeConstraint => true
        case SingletonTypeConstraint(t) => // can this happen?
          println(s"EqTypeConstraint $t"); satisfiedBy(typeEnv, t)
//        case MemberOf(ts) => ts.forall(t => satisfiedBy(typeEnv, t))
        case AnyTypeConstraint => false
      }
      case TypeParam(tp) => typeEnv.constraintForTypeParam(tp).implies(this)
      case _ => false // FunctionType
    }
  }

  def intersection(typeEnv: TypeEnv0, other: StoredTypeConstraint) = other match{
    case EqTypeConstraint => println("TypeConstraint.EqEq"); EqTypeConstraint
    // Note: above is currently untested
    case AnyTypeConstraint => EqTypeConstraint
  }

  /** Does this imply other?  I.e., the types that satisfy this are a subset of
    * the types that satisfy other? */
  def implies(other: StoredTypeConstraint): Boolean = other match{
    case EqTypeConstraint => true
    case AnyTypeConstraint => true  // In fact, never called
  }

  override def asString = "Eq" 

  def asStringE = "equality type"
}

// ==================================================================

/** The trivial type constraint, that allows all types. */
case object AnyTypeConstraint extends TypeParamConstraint{
  def satisfiedBy(typeEnv: TypeEnv0, t: TypeT) = true

  def intersection(typeEnv: TypeEnv0, other: StoredTypeConstraint) = other

  def implies(other: StoredTypeConstraint): Boolean = 
    other == AnyTypeConstraint
  // This only implies the same type constraint.

  def asStringE = "any type" // never used? 
}
