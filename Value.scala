package spreadsheet

/** Values represented by expressions. */
trait Value{
  /** The source this represents. */
  var source: Source = null

  /** Add source to this, and return this. */
  def withSource(s: Source) = { source = s; this }

  /** How this is presented in an error message. */
  def forError: String = toString
}

/** Values that can be in a cell. */
trait Cell extends Value{
  /** How this is presented in a cell. */
  def asCell: String = forError
}

/** Values that can be entered as user input. */
//trait UserValue extends Cell

/** An empty cell. */
case class Empty() extends Cell{
  override def asCell = ""

  override def forError = "empty cell"
}
// Note: we can't use a case object here, because different Emptys will have
// different sources.

/** An Int. */
case class IntValue(value: Int) extends Cell{
  override def forError = value.toString
}

case class StringValue(value: String) extends Cell{
  override def forError = s"\"$value\""

  override def asCell = value
}

case class BoolValue(value: Boolean) extends Value

// TO DO: add doubles, strings, lists, ...

case class RowValue(row: Int) extends Value

case class ColumnValue(column: Int) extends Value

// ========= Errors

trait ErrorValue extends Cell{
  def msg: String
}

/** A type error arising from evaluation of an expression. */
case class TypeError(msg: String) extends ErrorValue{
  override def forError = s"Type error: $msg"
}

/** An error arising from evaluation of an expression, such as division by
  * 0. */
case class EvalError(msg: String) extends ErrorValue
