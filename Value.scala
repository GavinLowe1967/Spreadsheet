package spreadsheet

/** Values represented by expressions. */
trait Value

/** Values that can be in a cell. */
trait Cell extends Value{
  def asCell: String = toString
}

/** Values that can be entered as user input. */
trait UserValue extends Cell

/** An empty cell. */
case object Empty extends Cell{
  override def asCell = ""
}

/** An Int. */
case class IntValue(value: Int) extends UserValue{
  //println("IntValue")
  override def asCell = value.toString
}

case class StringValue(value: String) extends UserValue{
  //println("StringValue")
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
case class TypeError(msg: String) extends ErrorValue

/** An error arising from evaluation of an expression, such as division by
  * 0. */
case class EvalError(msg: String) extends ErrorValue
