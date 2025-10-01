package spreadsheet

/** Representation of an expression. */
trait Exp extends HasExtent{
  /** Make an error message, saying that `found` was found when `expected` was
    * expected`. */
  private def mkErr(expected: String, found: Value): String = {
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
  def lift(f: PartialFunction[Value, Value], v: Value) : Value = 
    if(f.isDefinedAt(v)) f(v) else handleError(v)

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

  /** The type associated with this read of a cell.  It might be a TypeVar, in
  //   * which case the corresponding TypeEnv will have a constraint upon it. */
  // var theType: TypeT = null

  //def setType(t: TypeT) = ??? //  theType = t

  override def toString = s"Cell($column, $row): $theType"
}

// ==================================================================

/** An if expression `if(test) thenClause else elseClause`. */
case class IfExp(test: Exp, thenClause: Exp, elseClause: Exp) extends Exp

// =================================================================

case class ListLiteral(elems: List[Exp]) extends Exp

// ==================================================================

/** The application of a function represented by `f` to `args`. */
case class FunctionApp(f: Exp, args: List[Exp]) extends Exp

// =======================================================
// cell match expressions

/** A pattern in a cell match expression. */
trait Pattern

/** A pattern "name: theType". */
case class TypedPattern(name: NameExp.Name, theType: CellType) extends Pattern

/** A pattern "Empty". */
case object EmptyPattern extends Pattern

/** A pattern of the form "case pattern => body". */
case class MatchBranch(pattern: Pattern, body: Exp) extends HasExtent

/** An expression of the form "Cell(column, row) match{ branches }". */
case class CellMatchExp(column: Exp, row: Exp, branches: List[MatchBranch]) 
    extends Exp



// ========= Note =========
// Statement.scala contains another subclass, BlockExp.
