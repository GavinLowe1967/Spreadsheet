package spreadsheet

/** Representation of an expression. */
trait Exp extends HasExtent{

  /** Handle value v, which is expected to be an ErrorValue: lift it by tagging
    * on the extent of this. */
  protected def handleError(v: Value): ErrorValue = v match{ 
      case ev: ErrorValue => liftError(ev)
      case _ => sys.error(s"unexpected value: $v")
    }

  /** Extend f(v) to: cases where v is an ErrorValue (passing on the error).
    * Other cases shouldn't happen. */ 
  def lift(f: PartialFunction[Value, Value], v: Value) : Value = 
    if(f.isDefinedAt(v)) f(v) else handleError(v)

  /** If v is an error, lift it by tagging it with the extent of this. */
  def liftValue(v: Value, lineNum: Boolean = false): Value = v match{
    case err: ErrorValue => liftError(err, lineNum)
    case _ => v
  }
}

// ==================================================================

/** A name. */
case class NameExp(name: NameExp.Name) extends Exp{
  override def toString = name
}

object NameExp{
  /** The type of names of identifiers. */
  type Name = String
}

// ==================================================================

/** An integer constant expression. */
case class IntExp(value: Int) extends Exp{
  override def toString = value.toString
}

// ==================================================================

case class FloatExp(value: Float) extends Exp{
  override def toString = value.toString
}

// ==================================================================

/** A boolean constant. */
case class BoolExp(value: Boolean) extends Exp{
  override def toString = value.toString
}

// ==================================================================

/** A string literal. */
case class StringExp(value: String) extends Exp{
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

  // Note: the following is for testing only: it over-uses parentheses.
  override def toString = s"($left $op $right)"
}

// ==================================================================

/** A row literal. */
case class RowExp(row: Int) extends Exp{
  override def toString = s"#$row"
}

// ==================================================================

/** A column literal. */
case class ColumnExp(column: String) extends Exp{
  require(column.forall(_.isUpper))
  require(column.length <= 2) // surely? 

  /** Int representation of this. */
  val asInt = ColumnValue.asInt(column)

  override def toString = s"#$column"
}

// ==================================================================

/** A reference to a Cell.  Note: the coordinates are in the order
  * (column,row), matching standard spreadsheet usage.  `theType` gives the
  * expected type of the value in the cell. */
case class CellExp(column: Exp, row: Exp, theType: CellType) extends Exp{
  override def toString = s"Cell($column, $row): $theType"
}

/** An untyped cell expression. */
case class UntypedCellExp(column: Exp, row: Exp) extends Exp{
  /** The type of this cell.  Set by the typechecker. */
  private var theType: Option[CellType] = None
  def setType(t: CellType) = { 
    assert(theType == None || theType == Some(t), 
      s"Type of $this set for second time; $theType $t")
    theType = Some(t) 
  }
  def getType: CellType = theType.get

  /** The CellTypeVar used to represent the type of this cell during type
    * checking. */
  private var typeVar: Option[CellTypeVar] = None
  def setTypeVar(ctv: CellTypeVar) = { 
    assert(typeVar == None); typeVar = Some(ctv) 
  }
  def getTypeVar = typeVar.get
}

// =================================================== cell match expressions

/** A pattern in a cell match expression. */
trait Pattern{
  /** Does this pattern match type t? */
  def matches(t: TypeT): Boolean 
}

/** A pattern "name: theType". */
case class TypedPattern(name: NameExp.Name, theType: CellType) extends Pattern{
  def matches(t: TypeT) = t == theType
}

/** A pattern "Empty". */
case object EmptyPattern extends Pattern{
  def matches(t: TypeT) = t == EmptyType
}

/** A pattern of the form "case pattern => body". */
case class MatchBranch(pattern: Pattern, body: Exp) extends HasExtent

/** An expression of the form "Cell(column, row) match{ branches }". */
case class CellMatchExp(column: Exp, row: Exp, branches: List[MatchBranch]) 
    extends Exp

// ==================================================================

/** An if expression `if(test) thenClause else elseClause`. */
case class IfExp(test: Exp, thenClause: Exp, elseClause: Exp) extends Exp

// =================================================================

case class ListLiteral(elems: List[Exp]) extends Exp

// ==================================================================

/** The application of a function represented by `f` to `args`. */
case class FunctionApp(f: Exp, args: List[Exp]) extends Exp


// ========= Note =========
// Statement.scala contains another subclass, BlockExp.
