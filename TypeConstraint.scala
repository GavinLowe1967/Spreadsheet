package spreadsheet

/** A constraint upon a type variable. */
trait TypeConstraint

/** A type constraint representing a contradiction. */
case object EmptyTypeConstraint extends TypeConstraint

/** A type constraint representing a single type t. */
case class SingletonTypeConstraint(t: TypeT) extends TypeConstraint

/* Note: the above two cases can be returned by the intersection operation on
 * a StoredTypeConstraint, but are not themselves stored. */ 

// ==================================================================

/** A type constraint that can be stored against a type variable in a type
  * environment. */
trait StoredTypeConstraint extends TypeConstraint{
  /** Is this constraint satisfied by type t? */
  def satisfiedBy(t: TypeT) : Boolean

  /** The TypeConstraint representing the intersection (or conjunction) of this
    * and other. */
  def intersection(other: StoredTypeConstraint): TypeConstraint

  def asString: String
}

// ==================================================================

/** A constraint that a type variable represents a type within ts. */
case class MemberOf(ts: List[TypeT]) extends StoredTypeConstraint{
  require(ts.length >= 2)
  def satisfiedBy(t: TypeT) = ts.contains(t)

  def intersection(other: StoredTypeConstraint) = {
    val ts2 = ts.filter(other.satisfiedBy) 
    // println(s"$this $other $ts2")
    MemberOf.build(ts2)
  }

  def asString = ts.map(_.asString).mkString(" or ")
}

object MemberOf{

  def build(ts: List[TypeT]): TypeConstraint = 
    if(ts.isEmpty) EmptyTypeConstraint
    else if(ts.length == 1) SingletonTypeConstraint(ts.head)
    else MemberOf(ts)
}

// ==================================================================

case object EqTypeConstraint extends StoredTypeConstraint{
  def satisfiedBy(t: TypeT) = t.isEqType

  def intersection(other: StoredTypeConstraint) = other match{
    case EqTypeConstraint => println("TypeConstraint.EqEq"); EqTypeConstraint

    case MemberOf(ts) => println("TypeConstraint.EqM"); val ts1 = ts.filter(satisfiedBy); MemberOf.build(ts1)
  }
// Note: above is currently untested

  def asString = "equality type"
}

// case object NumTypeConstraint extends StoredTypeConstraint{
//   def satisfiedBy(t: TypeT) = t == IntType

//   def intersection(other: TypeConstraint) = other match{
//     case NumTypeConstraint => NumTypeConstraint
//   }
// }
