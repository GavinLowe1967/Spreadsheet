package spreadsheet

/** Values represented by expressions. */
trait Value{
  /** The source this represents. */
  var source: Source = null

  /** Add source to this, and return this. */
  def withSource(s: Source) = { source = s; this }

  /** How this is presented in an error message, and when in a cell that is
    * selected. */
  def forError: String // = toString
}

// ==================================================================

/** Values that can be in a cell. */
trait Cell extends Value{
  /** How this is presented in a cell.  Overwritten for StringValues. */
  def asCell: String = forError

  /** Text to display when this cell is selected. */ 
  def forSelection = source match{
    case CellWriteSource(_,_,d) =>
      val e = d.getExtent
      forError+s"\nFrom cell write at line ${e.lineNumber}:\n"+e.asString
    case _ => forError
  }

  /** How this is represented in a CSV file. */
  def asCSV: String = forError

  /** Add cs to this as the source, and return this. */
  private def withCSource(cs: Source): Cell = { source = cs; this }

  def withCellSource(c: Int, r: Int): Cell = withCSource(CellSource(c,r))

  def withCellWriteSource(c: Int, r: Int, d: Directive) = 
    withCSource(CellWriteSource(c, r, d))

  /** The type of this value.  Set in subclasses. */
  def getType: CellType // TypeT

  /** Is this an empty Cell? */
  def isEmpty = getType == EmptyType

  /** Is this a non-empty Cell? */
  def nonEmpty = !isEmpty
}


/** Types with + and - operators. */
trait Arith extends Value{
  def +(other: Arith): Value

  def -(other: Arith): Value
}

/** Values to which the "to" and "until" operations can be applied, to create
  * a range. */
trait Rangeable extends Arith{
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

  override def asCSV = ""
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

  def +(other: Arith) = other match{
    case IntValue(v1) => IntValue(value+v1)
  }

  def -(other: Arith) = other match{
    case IntValue(v1) => IntValue(value-v1)
  }

  override def forError = value.toString
}

// ==================================================================

/** A Float. */
case class FloatValue(value: Float) extends Cell with Arith{
  def +(other: Arith) = other match{
    case FloatValue(v1) => FloatValue(value+v1)
  }

  def -(other: Arith) = other match{
    case FloatValue(v1) => FloatValue(value-v1)
  }

  def getType = FloatType

  override def forError = value.toString
}

// ==================================================================

case class StringValue(value: String) extends Cell{
  def getType = StringType

  override def forError = s"\"$value\""

  override def asCell = value

  override def asCSV = 
    "\""+value.flatMap{ _ match{
      case '\"' => "\"\""; case c => s"$c"
    } }+"\""
}

// ==================================================================

case class BoolValue(value: Boolean) extends Cell{
  def getType = BoolType

  override def forError = value.toString
}

// ==================================================================

/** A Row with value `row`, 0-based. */
case class RowValue(row: Int) extends Value with Rangeable{
  require(row >= 0)

  def +(other: Arith) = other match{
    case IntValue(v) => RowValue(row+v)
  }

  def -(other: Arith) = other match{
    case IntValue(v) => 
      if(row >= v) RowValue(row-v) else EvalError("Negative row: "+(row-v))
  }

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
  require(column >= 0)

  def +(other: Arith) = other match{
    case IntValue(v) => ColumnValue(column+v)
  }

  def -(other: Arith) = other match{
    case IntValue(v) => 
      if(column >= v) ColumnValue(column-v) 
      else EvalError("Negative column: "+(column-v))
  }

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
  def getName(c: Int): String = CellSource.colName(c)
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

  def forError = toString // IMPROVE?
}

// ===========================================================  Errors

/** The union of error cases. */
trait ErrorValue extends Cell{
  def getType = ErrorType

  /** The message that is written in the view InfoBox. */
  def msg: String

  // Note: forError is the value that appears in the cell itself, and the
  // selection box.
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

/** A parsing error for a value entered in a cell. */
case class ParseError(msg: String) extends ErrorValue{
  override def forError = s"Parse error: $msg"

  override def asCSV = "" // Don't write this to CSV
// TODO think about this
}


/** A cell written to multiple times. */
case class MultipleWriteError(sources: List[Cell]) extends ErrorValue{
  def msg = 
    "Cell assigned multiple times.\n"+
      sources.map(v => v.source match{
        case CellWriteSource(_,_,d) => 
          val e = d.getExtent
          s"Value ${v.forError} from cell write at line ${e.lineNumber}: "+
            e.asString
        case cs: CellSource => s"User data: ${v.forError}"
        case null => s"null source: $v"
      }
      ).mkString("\n")

  def forError = msg
}

object MultipleWriteError{
  /** Factory method. */
  def apply(c1: Cell, c2: Cell) = c1 match{
    case MultipleWriteError(cells) => new MultipleWriteError(cells:+c2)
    case _ => new MultipleWriteError(List(c1,c2))
  }
}
