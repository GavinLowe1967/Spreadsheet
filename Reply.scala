package spreadsheet

/** A class representing results from typechecking. */
abstract class Reply[+A]{
  /** If successful, apply `f` to the contents of this. */
  def map[B](f: A => Reply[B]): Reply[B]

  /** If successful, apply `f` to the contents of this.  If a failure, add the
    * source of `exp` to the message. */
  def mapOrLift[B](exp: HasExtent, f: A => Reply[B]): Reply[B]

  /** If a failure, add the source of `exp` to the message. */
  def lift(exp: HasExtent, lineNum: Boolean = false): Reply[A]
}

// =======================================================

/** The result of a successful typechecking, corresponding to x. */
case class Ok[+A](x: A) extends Reply[A]{
  def map[B](f: A => Reply[B]) = f(x)

  def mapOrLift[B](exp: HasExtent, f: A => Reply[B]) = f(x)

  def lift(exp: HasExtent, lineNum: Boolean = false) = this
}

// =======================================================

/** The result of an unsuccessful typechecking, as explained by err. */
case class FailureR(err: String) extends Reply[Nothing]{
  def map[B](f: Nothing => Reply[B]) = this 

  def mapOrLift[B](exp: HasExtent, f: Nothing => Reply[B]) = lift(exp)

  /** Add the source of `exp` to the message. */
  def lift(exp: HasExtent, lineNum: Boolean = false) = {
    val extent = exp.getExtent
    assert(extent != null, s"Null extent in $exp")
    val lnString = if(lineNum) " at line "+extent.lineNumber else ""
    FailureR(err+lnString+"\nin "+extent.asString)
  }

  /** Add the line numbers from e1 and e2. */
  def addLines(e1: HasExtent, e2: HasExtent) = 
    FailureR(s"$err at lines ${e1.lineNumber} and ${e2.lineNumber}.")
}

object Reply{
  /** Fold with f along xs, starting with e, as long as f gives Ok results. */
  def fold[A,B](f: (A, B) => Reply[A], e: A, xs: List[B]): Reply[A] =
    if(xs.isEmpty) Ok(e)
    else f(e, xs.head).map(e1 => fold(f, e1, xs.tail))
}
