package spreadsheet

/** A constraint upon a type variable. */
trait TypeConstraint

/** A constraint that a type variable represents a type within ts. */
case class MemberOf(ts: List[TypeT]) extends TypeConstraint

