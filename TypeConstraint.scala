package spreadsheet


/** A constraint upon a type variable. */
trait TypeConstraint{
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
  * associated with type parameters of functions. They are created by
  * Unification.unify (via TypeEnv.replace) when a TypeVar is unified with a
  * concrete type. */
case class SingletonTypeConstraint(t: TypeT) extends TypeConstraint{
  def intersection(other: TypeConstraint) = other match{
    case AnyTypeConstraint => this
    case _ =>  println(s"$t $other"); ??? //FIXME
  }

  def asStringE = t.asString
}

// ==================================================================


/** The type constraint corresponding to being an equality type. */
case object EqTypeConstraint extends TypeParamConstraint{
  def intersection(other: TypeConstraint) = other match{
    case OrdTypeConstraint => OrdTypeConstraint
    case CellTypeConstraint => CellTypeConstraint
    case EqTypeConstraint | AnyTypeConstraint => EqTypeConstraint
      // tested by  applyE(threeE, true) in TypeCheckerTest2
    case SingletonTypeConstraint(_) => ???
  }

  override def asString = "Eq" 

  def asStringE = "equality type"
}

// =================================================================

case object OrdTypeConstraint extends TypeParamConstraint{
  def intersection(other: TypeConstraint) = other match{
    case CellTypeConstraint => CellTypeConstraint
    case EqTypeConstraint | OrdTypeConstraint | AnyTypeConstraint => 
      OrdTypeConstraint
    case SingletonTypeConstraint(_) => ???
  }

  override def asString = "Ord" 

  def asStringE = "Ord type"
}

// ==================================================================

/** The trait of types that can appear in cells. */
case object CellTypeConstraint extends TypeParamConstraint{
  def intersection(other: TypeConstraint) = other match{
    case CellTypeConstraint | EqTypeConstraint | OrdTypeConstraint | 
        AnyTypeConstraint => CellTypeConstraint
    case _ => ???
  }

  def asStringE = "CellType"
}

// ==================================================================

/** The trivial type constraint, that allows all types. */
case object AnyTypeConstraint extends TypeParamConstraint{
  def intersection(other: TypeConstraint) = other

  def asStringE = "any type" // never used? 
}
