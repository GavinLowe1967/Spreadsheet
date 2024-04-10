package spreadsheet

/* Note: this class can't be included in Value.scala, because it builds on
 * Environment. */

/** A value of a function, params => body: rt, to be evaluated in environment
  * env.   */
case class FunctionValue(
  params: List[(String,TypeT)], rt: TypeT, body: Exp, env: Environment)
    extends Value
