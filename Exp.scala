package spreadsheet

/** The result of a parse with an Extent. */
trait HasExtent{
  /** The Extent representing the string from which this was produced. */
  protected var extent: Extent = null

  def getExtent = extent

  /** Set the Extent representing the string from which this was produced. */
  def setExtent(e: Extent) = extent = e  

  protected val tab = "     "

  /** Lift an error value, by tagging on the extent of this. 
    * @param lineNum if true, include line number. */
  def liftError(error: ErrorValue, lineNum: Boolean = false) = {
    assert(extent != null, s"Null extent in $this")
    val lnString = if(lineNum) " at line "+extent.lineNumber else ""
    def extend(msg: String) = s"$msg$lnString\n${tab}in \"${extent.asString}\""
    error match{
      case TypeError(msg) => TypeError(extend(msg))
      case EvalError(msg) => EvalError(extend(msg))
    }
  }
}

// =======================================================

/** Representation of an expression. */
trait Exp extends HasExtent{
  /** Evaluate this in environment `env`. */
  def eval0(env: Environment): Value

  /** Evaluate this in environment `env`, adding the extent from this. */
  def eval(env: Environment): Value = eval0(env).withSource(extent)

  /** Make an error message, saying that `found` was found when `expected` was
    * expected`. */
  protected def mkErr(expected: String, found: Value): String = {
    val source = found.source; assert(source != null, s"No source for $found")
    s"Expected $expected, found ${found.forError} in "+
    (source match{
      case cs: CellSource => cs.asString       // don't include quotes here
      case ex: Extent => s"\"${ex.asString}\"" // but do here
    }) +
    s"\n${tab}in \"${extent.asString}\""
  }

  /** Handle value v, which is expected to be an ErrorValue: lift it by tagging
    * on the extent of this. */
  protected def handleError(v: Value): ErrorValue = v match{ 
      case ev: ErrorValue => liftError(ev)
      case _ => sys.error(s"unexpected value: $v")
    }

  /** Extend f(v) to: cases where v is an ErrorValue (passing on the error).
    * Other cases shouldn't happen. */ 
  protected def lift(f: PartialFunction[Value, Value], v: Value) : Value = 
    if(f.isDefinedAt(v)) f(v) else handleError(v)

}

// ==================================================================

/** A name. */
case class NameExp(name: NameExp.Name) extends Exp{
  def eval0(env: Environment) = {
    assert(extent != null, s"Null extent in $this")
    env.get(name) match{
      case Some(value) => value 
      case None => sys.error(s"Name not found: $name")
    }
  }

  override def toString = name
}

object NameExp{
  /** The type of names of identifiers. */
  type Name = String
}

// ==================================================================

/** An integer constant expression. */
case class IntExp(value: Int) extends Exp{
  def eval0(env: Environment) = IntValue(value) 

  override def toString = value.toString
}

// ==================================================================

case class FloatExp(value: Float) extends Exp{
  def eval0(env: Environment) = FloatValue(value)

  override def toString = value.toString
}

// ==================================================================

/** A boolean constant. */
case class BoolExp(value: Boolean) extends Exp{
  def eval0(env: Environment) = BoolValue(value)

  override def toString = value.toString
}

// ==================================================================

/** A string literal. */
case class StringExp(value: String) extends Exp{
  def eval0(env: Environment) = StringValue(value)

  override def toString = value
}

// ==================================================================
 
/** An application of a binary operator. */
case class BinOp(left: Exp, op: String, right: Exp) extends Exp{
  // Set extent. 
  if(left.getExtent != null && right.getExtent != null)
    // Note: the guard will hold for expressions created by the parser.
    extent = left.getExtent.until(right.getExtent)
  // Note: extent might be overwritten if the corresponding syntax is in
  // parentheses.  This is normally what we want. 

  def eval0(env: Environment) = {
    assert(left.getExtent != null, left.toString)
    assert(right.getExtent != null, "right"+right.toString)
    assert(extent != null)
    doBinOp(left.eval(env), op, right.eval(env)) 
  }

  /** Shorthand for a partial function Value => A. */
  type PF[A] = PartialFunction[Value, A]

  /** Representation of a binary operator as a curried partial function. */
  private type BinOpRep = PF[PF[Value]]

  /* The functions below define BinOpReps by lifting functions over the
   * primitive types. */

  /** (Bool,Bool) -> Bool functions. */
  private def mkBoolOp(f: (Boolean,Boolean) => Boolean): BinOpRep = {
    case BoolValue(b1) => { case BoolValue(b2) => BoolValue(f(b1,b2)) }
  }

  /** (Num, Num) -> Value functions. */
  private def mkBinOp(fi: (Int,Int) => Value, ff: (Float,Float) => Value) 
      : BinOpRep = {
    case IntValue(n1) => { case IntValue(n2) => fi(n1,n2) }
    case FloatValue(x1) => { case FloatValue(x2) => ff(x1,x2) }
  }

  /** (Num, Num) -> Num functions. */
  private def mkBinNumOp(fi: (Int,Int) => Int, ff: (Float,Float) => Float)
      : BinOpRep =
    mkBinOp({case (n1,n2) => IntValue(fi(n1,n2))},
            {case (x1,x2) => FloatValue(ff(x1,x2))} )

  /** (Num, Num) -> Bool functions. */
  private def mkBinRelOp(fi: (Int,Int) => Boolean, ff: (Float,Float) => Boolean)
      : BinOpRep =
    mkBinOp({case (n1,n2) => BoolValue(fi(n1,n2))}, 
            {case (x1,x2) => BoolValue(ff(x1,x2))} )

  /** Equality operators.
    * @param eq Is this representing the equality operator (==), as opposed to
    * inequality (!=)? */
  private def equalOp(eq: Boolean): BinOpRep = {
    // Build the result from b which represents equality. 
    def mkRes(b: Boolean) = BoolValue(b == eq)
    _ match {
      case IntValue(n1) => { case IntValue(n2) => mkRes(n1 == n2) }
      case FloatValue(x1) => { case FloatValue(x2) => mkRes(x1 == x2) }
      case BoolValue(b1) => { case BoolValue(b2) => mkRes(b1 == b2) }
      case StringValue(st1) => { case StringValue(st2) => mkRes(st1 == st2) }
      case RowValue(r1) => { case RowValue(r2) => mkRes(r1 == r2) }
      case ColumnValue(c1) => { case ColumnValue(c2) => mkRes(c1 == c2) }
      case ListValue(elems1) => { case ListValue(elems2) => 
        mkRes(elems1 == elems2) } // Note: lists can contain no error values
    }
  }

  /** Representation of the cons (::) operator. */
  private def consOp: BinOpRep = {
    case (v: Value) => 
      { case ListValue(vs) => /*assert(v.isOfType(t));*/ ListValue(v::vs) }
  }

  /** Apply the operation represented by `op` to values `v1` and `v2`. */
  private def doBinOp(v1: Value, op: String, v2: => Value): Value = {
    // The representation of op as a BinOpRep
    val f : BinOpRep = op match{
      case "+" => mkBinNumOp((_+_), (_+_)); case "-" => mkBinNumOp((_-_), (_-_))
      case "*" => mkBinNumOp((_*_), (_*_))
      case "/" => 
        def err = liftError(EvalError("Division by zero"), true) // inc line num
        mkBinOp({case (n1,n2) => if(n2 != 0) IntValue(n1/n2) else err},
          {case (x1,x2) => if(x2 != 0.0) FloatValue(x1/x2) else err} )
      case "<=" => mkBinRelOp((_<=_), (_<=_))
      case "<" => mkBinRelOp((_<_), (_<_))
      case ">=" => mkBinRelOp((_>=_), (_>=_))
      case ">" => mkBinRelOp((_>_), (_>_))
      case "&&" => mkBoolOp((_&&_)); case "||" => mkBoolOp((_||_))
      case "==" => equalOp(true); case "!=" => equalOp(false)
      case "::" => consOp
    }
    if(f.isDefinedAt(v1)){ val f1 = f(v1); lift(f1, v2) }
    else handleError(v1)
  }



  // Note: the following is for testing only: it over-uses parentheses.
  override def toString = s"($left $op $right)"
}


// ==================================================================

/** A row literal. */
case class RowExp(row: Int) extends Exp{
  def eval0(env: Environment) = RowValue(row) 

  override def toString = s"#$row"
}

// ==================================================================

/** A column literal. */
case class ColumnExp(column: String) extends Exp{
  require(column.forall(_.isUpper))
  require(column.length <= 2) // surely? 

  def eval0(env: Environment) = ColumnValue(asInt)

  /** Int representation of this. */
  private val asInt = ColumnValue.asInt(column)

  override def toString = s"#$column"
}


// ==================================================================

/** A reference to a Cell.  Note: the coordinates are in the order
  * (column,row), matching standard spreadsheet usage. */
case class CellExp(column: Exp, row: Exp) extends Exp{
  def eval0(env: Environment) = ??? 
  // Note: above never called, since env.getCell sets the source

  override def eval(env: Environment) = {
    /* Read from cell(c,r). */
    def doRead(c: Int, r: Int) = {
      assert(theType != null); val v = env.getCell(c, r)
      env.checkType(v, theType) match{
        case Ok(()) => v; 
        case FailureR(msg) => 
          val cName = ColumnValue.getName(c)
          liftError(TypeError(msg+s" in cell (#$cName,#$r)" ))
      }
    }
    val cc = column.eval(env)
    lift({ case ColumnValue(c) => 
      val rr = row.eval(env); lift({ case RowValue(r) => doRead(c,r) }, rr)
    }, cc)
  }

  /** The type associated with this read of a cell.  It might be a TypeVar, in
    * which case the corresponding TypeEnv will have a constraint upon it. */
  private var theType: TypeT = null

  def setType(t: TypeT) = theType = t 

  override def toString = s"Cell($column, $row)"
}

// ==================================================================

/** An if expression `if(test) thenClause else elseClause`. */
case class IfExp(test: Exp, thenClause: Exp, elseClause: Exp) extends Exp{
  def eval0(env: Environment) = test.eval(env) match{
    case BoolValue(true) => thenClause.eval(env)
    case BoolValue(false) => elseClause.eval(env)
    case err: ErrorValue => liftError(err)
    case other => sys.error(s"Unexpected type: $other")
  }
}

// =================================================================

case class ListLiteral(elems: List[Exp]) extends Exp{
  def eval0(env: Environment) = {
    // Traverse elems, evaluating each, and building in vs in reverse;
    // maintain type of elements in theType; and catch any error.
    var es = elems; var vs = List[Value](); var error: ErrorValue = null
    // var theType: TypeT = AnyType                       // FIXME
    while(es.nonEmpty && error == null){
      es.head.eval(env) match{
        case err: ErrorValue => error = liftError(err)
        case v => vs ::= v; /*theType = v.getType;*/ es = es.tail
          // if(v.isOfType(theType)){ vs ::= v; theType = v.getType; es = es.tail }
          // else sys.error(mkErr(theType.asString, v))
      }
    }
    if(error != null) error else ListValue(vs.reverse)
  }

  // private var underlyingType: TypeT = null

  // def setUnderlyingType(t: TypeT) = underlyingType = t
}

// ==================================================================

object Exp{

}

// ========= Note =========
// FunctionValue.scala contains another subclass, FunctionApp, and 
// BlockExp.scala contains another subclass, BlockExp.
