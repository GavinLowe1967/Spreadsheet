package spreadsheet

/** The interface of TypeEnv as seen by a TypeConstraint, namely the ability
  * to find the TypeConstraint stored against a TypeID.
  * Note: this is used to avoid cyclic compilation dependencies. */
trait TypeEnv0{
  /** The constraint associated with tid. */
  def apply(tid: TypeVar.TypeID) : StoredTypeConstraint
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

  def asString: String
}

// ==================================================================

/** A type constraint representing a single type t.  Note: these are stored
  * when the value of a type variable is completely decided. */
case class SingletonTypeConstraint(t: TypeT) extends StoredTypeConstraint{
  def satisfiedBy(typeEnv: TypeEnv0, t1: TypeT) = t1 == t

  def intersection(typeEnv: TypeEnv0, other: StoredTypeConstraint) = ???

  def asString = t.asString
}

// ==================================================================

/** A constraint that a type variable represents a type within ts. */
case class MemberOf(ts: List[TypeT]) extends StoredTypeConstraint{
  require(ts.length >= 2)
  def satisfiedBy(typeEnv: TypeEnv0, t: TypeT) = ts.contains(t)

  def intersection(typeEnv: TypeEnv0, other: StoredTypeConstraint) = {
    val ts2 = ts.filter(t => other.satisfiedBy(typeEnv, t)) 
    // println(s"$this $other $ts2")
    MemberOf.build(ts2)
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
    }
  }

  def intersection(typeEnv: TypeEnv0, other: StoredTypeConstraint) = other match{
    case EqTypeConstraint => println("TypeConstraint.EqEq"); EqTypeConstraint

    case MemberOf(ts) => 
      println("TypeConstraint.EqM"); 
      val ts1 = ts.filter(t => satisfiedBy(typeEnv, t)); MemberOf.build(ts1)
  }
// Note: above is currently untested

  def asString = "equality type"
}

// ==================================================================

/** The trivial type constraint, that allows all types. */
case object AnyTypeConstraint extends StoredTypeConstraint{
  def satisfiedBy(typeEnv: TypeEnv0, t: TypeT) = true

  def intersection(typeEnv: TypeEnv0, other: StoredTypeConstraint) = other

  def asString = ??? // never used? 
}
