package spreadsheet


/** Values represented by expressions. */
trait Value{
  /** The source this represents. */
  var source: Source = null

  /** Add source to this, and return this. */
  def withSource(s: Source) = { source = s; this }

  /** How this is presented in an error message. */
  def forError: String = toString

  /** The type of this value.  Set in subclasses. */
  protected val theType: TypeT // = null // FIXME - require subclasses to define

  /** Is the type of this a subclass of `t`? */
  def isOfType(t: TypeT) = {
    require(theType != null, s"$this $t"); 
    theType.isSubclassOf(t) //  == AnyType || t == theType
  }
  // IMPROVE: consider subclassing

  def getType = theType

}

// ==================================================================

/** Values that can be in a cell. */
trait Cell extends Value{
  /** How this is presented in a cell. */
  def asCell: String = forError
}

// ==================================================================

/** An empty cell. */
case class Empty() extends Cell{
  protected val theType = EmptyType

  override def asCell = ""

  override def forError = "empty cell"
}
// Note: we can't use a case object here, because different Emptys will have
// different sources.

// ==================================================================

/** An Int. */
case class IntValue(value: Int) extends Cell{
  protected val theType = IntType

  override def forError = value.toString
}

// ==================================================================

/** A Float. */
case class FloatValue(value: Float) extends Cell{
  protected val theType = FloatType

  override def forError = value.toString
}

// ==================================================================

case class StringValue(value: String) extends Cell{
  protected val theType = StringType

  override def forError = s"\"$value\""

  override def asCell = value
}

// ==================================================================

case class BoolValue(value: Boolean) extends Cell{
  protected val theType = BoolType

  override def forError = value.toString
}

// ==================================================================

// TO DO: add doubles, strings, lists, ...

// ==================================================================

case class RowValue(row: Int) extends Value{
  protected val theType = RowType

  override def forError = s"#$row"
}

// =======================================================

case class ColumnValue(column: Int) extends Value{
  protected val theType = ColumnType

  override def forError = "#"+CellSource.colName(column)
}

object ColumnValue{

  /** Convert column to the corresponding Int representation. */
  def asInt(column: String): Int = 
    if(column.length == 1) column(0)-'A' 
    else (column(0)-'A'+1)*26 + column(1)-'A'

  /** The name for the collumn with index c. */
  def getName(c: Int): String = {
    def toChar(n: Int) = (n+'A').toChar
    if(c < 26) toChar(c).toString else List(toChar(c/26),toChar(c%26)).mkString
  }

}

// =======================================================

/** A List value. */
case class ListValue(underlying: TypeT, elems: List[Value]) extends Value{
  assert(elems.forall(v => ! v.isInstanceOf[ErrorValue]))

  protected val theType = ListType(underlying) // FIXME

  override def forError = 
    s"List[${underlying.asString}]"+
      elems.map(_.forError).mkString("(", ", ", ")")

  /* Functions corresponding to built-in functions. */
  def head = if(elems.nonEmpty) elems.head else EvalError("head of empty list")

  def tail: Value = 
    if(elems.nonEmpty) ListValue(underlying, elems.tail) 
    else EvalError("tail of empty list")
}

// ==================================================================

/** The value of a built-in function of type `paramTypes => rt`, defined by
  * `f`. */
case class BuiltInFunction(paramTypes: List[TypeT], rt: TypeT)
  (f: PartialFunction[List[Value], Value]) 
    extends Value{

  protected val theType = FunctionType(paramTypes, rt)

  /** Apply this to `args`. */
  def apply(args: List[Value]): Value = 
    if(f.isDefinedAt(args)) f(args)
    else ??? // FIXME catch errors
}

object BuiltInFunction{

  // IMPROVE: think about types

  private val headFn = 
    BuiltInFunction(List(ListType(AnyType)), AnyType){
      case List(l:ListValue) => l.head
    }
  private val tailFn = 
    BuiltInFunction(List(ListType(AnyType)), ListType(AnyType)){
      case List(l:ListValue) => l.tail
    }

  /** The built-in functions. */
  val builtIns = List("head" -> headFn, "tail" -> tailFn) 
}


// ========= Errors

trait ErrorValue extends Cell{
  protected val theType = null // IMPROVE? 

  def msg: String
}

/** A type error arising from evaluation of an expression. */
case class TypeError(msg: String) extends ErrorValue{
  override def forError = s"Type error: $msg\n\t$source"
}

/** An error arising from evaluation of an expression, such as division by
  * 0. */
case class EvalError(msg: String) extends ErrorValue{
  override def forError = s"Evaluation error: $msg"
}


//========= Note =========
// FunctionValue contains another subclass of Value, FunctionValue.
