package spreadsheet

/** The interface of TypeEnv as seen by a TypeConstraint, namely the ability
  * to find the TypeConstraint stored against a TypeID.
  * Note: this is used to avoid cyclic compilation dependencies. */
trait TypeEnv0{
  /** The constraint associated with TypeVar(tid). */
  def apply(tid: TypeVar.TypeID) : StoredTypeConstraint

  /** The constraint associated with TypeParam(tp). */
  def constraintForTypeParam(tp: TypeParam.TypeParamName) : StoredTypeConstraint
}

// ==================================================================

/** A constraint upon a type variable. */
trait TypeConstraint

/** A type constraint representing a contradiction. */
case object EmptyTypeConstraint extends TypeConstraint

/* Note: the above case can be returned by the intersection operation on a
 * StoredTypeConstraint, but is not itself stored. */ 

// ==================================================================

/** A type constraint that can be stored against a type variable in a type
  * environment. */
trait StoredTypeConstraint extends TypeConstraint{
  /** Is this constraint satisfied by type t? */
  def satisfiedBy(typeEnv: TypeEnv0, t: TypeT) : Boolean

  /** The TypeConstraint representing the intersection (or conjunction) of this
    * and other. */
  def intersection(typeEnv: TypeEnv0, other: StoredTypeConstraint)
      : TypeConstraint

  /** Does this imply other?  I.e., the types that satisfy this are a subset of
    * the types that satisfy other? */
  def implies(other: StoredTypeConstraint): Boolean

  /** String to use in error messages when this constraint is found. */
  def asString: String

  /** String to use in error messages when this type is expected.  Overwritten
    * in EqTypeConstraint. */
  def asStringE = asString
}

//trait TypeParamConstraint extends StoredTypeConstraint{

  // /** Does this imply other?  I.e., the types that satisfy this are a subset of
  //   * the types that satisfy other? */
  // def implies(other: TypeParamConstraint): Boolean
//}

// ==================================================================

/** A type constraint representing a single type t.  Note: these are stored
  * when the value of a type variable is completely decided. */
case class SingletonTypeConstraint(t: TypeT) extends StoredTypeConstraint{
  def satisfiedBy(typeEnv: TypeEnv0, t1: TypeT) = t1 == t

  def intersection(typeEnv: TypeEnv0, other: StoredTypeConstraint) = ??? //FIXME

  def implies(other: StoredTypeConstraint): Boolean = ??? //FIXME

  def asString = t.asString
}

// ==================================================================

/** A constraint that a type variable represents a type within ts. */
case class MemberOf(ts: List[TypeT]) extends StoredTypeConstraint{
  require(ts.length >= 2)
  def satisfiedBy(typeEnv: TypeEnv0, t: TypeT) = t match{
    case TypeVar(_) => ???
    case TypeParam(tp) => 
      val c = typeEnv.constraintForTypeParam(tp)
      // println(s"$tp -> $c")
      c.implies(this)

    case _ => ts.contains(t)
  }

  def intersection(typeEnv: TypeEnv0, other: StoredTypeConstraint) = {
    val ts2 = ts.filter(t => other.satisfiedBy(typeEnv, t)) 
    // println(s"$this $other $ts2")
    MemberOf.build(ts2)
  }

  def implies(other: StoredTypeConstraint): Boolean = {
    // println(s"$this implies $other")
    other match{
      case MemberOf(ts1) => ts.forall(ts1.contains(_))
      case EqTypeConstraint => true
      case _ => ??? // Never called
    }
  }

  def asString = ts.map(_.asString).mkString(" or ")
}

object MemberOf{
  /** Build a constraint corresponding to the options in ts. */
  def build(ts: List[TypeT]): TypeConstraint = 
    if(ts.isEmpty) EmptyTypeConstraint
    else if(ts.length == 1) SingletonTypeConstraint(ts.head)
    else MemberOf(ts)
}

// ==================================================================

/** The type constraint corresponding to being an equality type. */
case object EqTypeConstraint extends StoredTypeConstraint{
  def satisfiedBy(typeEnv: TypeEnv0, t: TypeT) = t match{
    case _: EqType => true
    case ListType(underlying) => satisfiedBy(typeEnv, underlying)
    case _: FunctionType => false
    case TypeVar(tid) => typeEnv(tid) match{
      case EqTypeConstraint => true
      case SingletonTypeConstraint(t) => // can this happen?
        println(s"EqTypeConstraint $t"); satisfiedBy(typeEnv, t) 
      case MemberOf(ts) => ts.forall(t => satisfiedBy(typeEnv, t))
      case AnyTypeConstraint => false
      case c => println(c);  ??? // FIXME
    }
    case TypeParam(tp) => 
      val c = typeEnv.constraintForTypeParam(tp)
      // println(s"$tp -> $c")
      c.implies(this)
  }

  def intersection(typeEnv: TypeEnv0, other: StoredTypeConstraint) = other match{
    case EqTypeConstraint => println("TypeConstraint.EqEq"); EqTypeConstraint

    case MemberOf(ts) => 
      println("TypeConstraint.EqM"); 
      val ts1 = ts.filter(t => satisfiedBy(typeEnv, t)); MemberOf.build(ts1)
  }
// Note: above is currently untested

  def implies(other: StoredTypeConstraint): Boolean = other match{
    case EqTypeConstraint => true
    case AnyTypeConstraint => true
    case MemberOf(ts) => false // ts can't contain Lists
    case _ => println(other); ??? // FIXME
  }

  def asString = "Eq" 

  override def asStringE = "equality type"
}

// ==================================================================

/** The trivial type constraint, that allows all types. */
case object AnyTypeConstraint extends StoredTypeConstraint{
  def satisfiedBy(typeEnv: TypeEnv0, t: TypeT) = true

  def intersection(typeEnv: TypeEnv0, other: StoredTypeConstraint) = other

  def implies(other: StoredTypeConstraint): Boolean = 
    other == AnyTypeConstraint
  // This only implies the same type constraint.

  def asString = ??? // never used? 
}
