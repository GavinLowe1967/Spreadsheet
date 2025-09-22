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

// ==================================================================

/** Values that can be in a cell. */
trait Cell extends Value{
  /** How this is presented in a cell. */
  def asCell: String = forError

  /** Add cs to this as the source, and return this. */
  def withCellSource(cs: CellSource) = { source = cs; this }

  /** The type of this value.  Set in subclasses. */
  def getType: CellType // TypeT
}

// object Cell{
//   /** Produce a Cell corresponding to st. */
//   def fromString(st: String): Cell = Empty() // FIXME
// }

/** Values to which the "to" and "until" operations can be applied, to create
  * a range. */
trait Rangeable extends Value{
  /** The ListValue containing elements from this up to and including other. */
  def to(other: Rangeable): ListValue

  /** The ListValue containing elements from this up to but excluding including
    * other. */
  def until(other: Rangeable): ListValue
}
// ==================================================================

/** An empty cell. */
case class Empty() extends Cell{
  def getType = EmptyType

  override def asCell = ""

  override def forError = "empty cell"
}
// Note: we can't use a case object here, because different Emptys will have
// different sources.

// ==================================================================

/** An Int. */
case class IntValue(value: Int) extends Cell with Rangeable{
  def getType = IntType

  def to(other: Rangeable) = other match{
    case IntValue(v1) => ListValue((value to v1).map(IntValue).toList)
  }

  def until(other: Rangeable) = other match{
    case IntValue(v1) => ListValue((value until v1).map(IntValue).toList)
  }

  override def forError = value.toString
}

// ==================================================================

/** A Float. */
case class FloatValue(value: Float) extends Cell{
  def getType = FloatType

  override def forError = value.toString
}

// ==================================================================

case class StringValue(value: String) extends Cell{
  def getType = StringType

  override def forError = s"\"$value\""

  override def asCell = value
}

// ==================================================================

case class BoolValue(value: Boolean) extends Cell{
  def getType = BoolType

  override def forError = value.toString
}

// ==================================================================

/** A Row with value `row`, 0-based. */
case class RowValue(row: Int) extends Value with Rangeable{
  def to(other: Rangeable) = other match{
    case RowValue(r1) => ListValue((row to r1).map(RowValue).toList)
  }

  def until(other: Rangeable) = other match{
    case RowValue(r1) => ListValue((row until r1).map(RowValue).toList)
  }

  override def forError = s"#$row"
}

// =======================================================

/** A Column with value `column`, where 0 represents #A, etc. */
case class ColumnValue(column: Int) extends Value with Rangeable{
  def to(other: Rangeable) = other match{
    case ColumnValue(c1) => ListValue((column to c1).map(ColumnValue(_)).toList)
  }

  def until(other: Rangeable) = other match{
    case ColumnValue(c1) => 
      ListValue((column until c1).map(ColumnValue(_)).toList)
  }

  override def forError = "#"+CellSource.colName(column)
}

object ColumnValue{
  /** Convert column to the corresponding Int representation. */
  def asInt(column: String): Int = {
    require(column.forall{ c => 'A' <= c && c<= 'Z' })
    if(column.length == 1) column(0)-'A' 
    else (column(0)-'A'+1)*26 + column(1)-'A'
  }

  /** The name for the collumn with index c. */
  def getName(c: Int): String = {
    def toChar(n: Int) = (n+'A').toChar
    if(c < 26) toChar(c).toString else List(toChar(c/26),toChar(c%26)).mkString
  }
}

// =======================================================

/** A List value. */
case class ListValue(elems: List[Value]) extends Value{
  assert(elems.forall(v => !v.isInstanceOf[ErrorValue]))

  override def forError = 
    s"List"+elems.map(_.forError).mkString("(", ", ", ")")

  /* Functions corresponding to built-in functions. */
  def head = if(elems.nonEmpty) elems.head else EvalError("head of empty list")

  def tail: Value = 
    if(elems.nonEmpty) ListValue(elems.tail) 
    else EvalError("tail of empty list")

  def isEmpty: Value = BoolValue(elems.isEmpty)
}

object ListValue{
  /** Convenience factory method. */
  def apply(vs: Value*) = new ListValue(vs.toList)
}

// ==================================================================

/** A function defined by `f`. */
case class FunctionValue(f: PartialFunction[List[Value], Value]) extends Value{
  /** Apply this to `args`. */
  def apply(args: List[Value]): Value = {
    assert(f.isDefinedAt(args)); f(args)
  }
}

// ===========================================================  Errors

trait ErrorValue extends Cell{
  def getType = null // IMPROVE? 

  def msg: String
}

/** A type error arising from evaluation of an expression. */
case class TypeError(msg: String) extends ErrorValue{
  override def forError = s"Type error: $msg" // \n\t$source"
}

/** An error arising from evaluation of an expression, such as division by
  * 0. */
case class EvalError(msg: String) extends ErrorValue{
  override def forError = s"Evaluation error: $msg"
}
