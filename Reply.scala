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

/** The result of a successful typechecking, corresponding to x. */
case class Ok[+A](x: A) extends Reply[A]{
  def map[B](f: A => Reply[B]) = f(x)

  def mapOrLift[B](exp: HasExtent, f: A => Reply[B]) = f(x)

  def lift(exp: HasExtent, lineNum: Boolean = false) = this
}

/** The result of an unsuccessful typechecking, as explained by err. */
case class FailureR(err: String) extends Reply[Nothing]{
  def map[B](f: Nothing => Reply[B]) = this // FailureR(err)

  def mapOrLift[B](exp: HasExtent, f: Nothing => Reply[B]) = lift(exp)
  //  FailureR(err+"\n\tin "+exp.getExtent.asString)

  /** Add the source of `exp` to the message. */
  def lift(exp: HasExtent, lineNum: Boolean = false) = {
    val extent = exp.getExtent
    val lnString = if(lineNum) " at line "+extent.lineNumber else ""
    FailureR(err+lnString+"\n\tin "+extent.asString)
  }
}
