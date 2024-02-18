package spreadsheet

/** The result of a parse with an Extent. */
trait HasExtent{
  /** The Extent representing the string from which this was produced. */
  protected var extent: Extent = null

  def getExtent = extent

  /** Set the Extent representing the string from which this was produced. */
  def setExtent(e: Extent) = extent = e  

  /** Lift an error value, by tagging on the extent of this. */
  def liftError(error: ErrorValue) = error match{
    case TypeError(msg) => TypeError(s"$msg\n\tin \"${extent.asString}\"")
    case EvalError(msg) => EvalError(s"$msg\n\tin \"${extent.asString}\"")
  }
}

// =======================================================

/** Representation of an expression. */
trait Exp extends HasExtent{
  /** Evaluate this in environment `env`. */
  def eval(env: Environment): Value

  /** Make an error message, saying that `found` was found when `expected` was
    * expected`. */
  protected def mkErr(expected: String, found: Value): String = {
    val source = found.source; assert(source != null, s"No source for $found")
    s"Expected $expected, found ${found.forError} in "+
    (source match{
      case cs: CellSource => cs.asString       // don't include quotes here
      case ex: Extent => s"\"${ex.asString}\"" // but do here
    }) +
    //s"\"${source.asString}\"
    s"\n\tin \"${extent.asString}\""
  }

  /** Extend f(v) to: (1) cases where v is an ErrorValue (passing on the error),
    * and (2) other types, returning a TypeError(err). */ 
  protected def lift(f: PartialFunction[Value, Value], v: Value, err: String)
      : Value = {
    if(f.isDefinedAt(v)) f(v) 
    else v match{ 
      case ev: ErrorValue => liftError(ev); 
      case _ => TypeError(err)
    }
  }
}

// ==================================================================

/** A name. */
case class NameExp(name: String) extends Exp{
  def eval(env: Environment) = env.get(name) match{
    case Some(value) => value.withSource(extent)
    case None => EvalError(s"Name not found: $name").withSource(extent)
  }

  override def toString = name
}

// ==================================================================

/** An integer expression. */
case class IntExp(value: Int) extends Exp{
  def eval(env: Environment) = IntValue(value).withSource(extent)

  override def toString = value.toString
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

  def eval(env: Environment) = {
    assert(left.getExtent != null, left.toString)
    assert(right.getExtent != null, "right"+right.toString)
    assert(extent != null)
    doBinOp(left.eval(env), op, right.eval(env)).withSource(extent)
  }

  //import Exp.{lift}

  /** Apply the operation represented by `op` to values `v1` and `v2`. */
  private def doBinOp(v1: Value, op: String, v2: Value): Value = (op match{
    // (Int, Int) operators
    case "+" | "-" | "*" | "/" | "==" | "!=" | "<" | "<=" | ">" | ">=" =>
      // println(s"$v1  ${v1.source} $op  $v2  ${v2.source}")
      lift(_ match{
        case IntValue(n1) => 
          lift(_ match{
            case IntValue(n2) => op match{
              case "+" => IntValue(n1+n2); case "-" => IntValue(n1-n2)
              case "*" => IntValue(n1*n2)
              case "/" =>
                if(n2 == 0) EvalError("Division by zero") else IntValue(n1/n2)
              case "==" => BoolValue(n1 == n2); case "!=" => BoolValue(n1 != n2)
              case "<" => BoolValue(n1 < n2); case "<=" => BoolValue(n1 <= n2)
              case ">" => BoolValue(n1 > n2); case ">=" => BoolValue(n1 >= n2)
            } // end of op match
          }, // end of anonymous match
          v2, mkErr("Int", v2)) // end of inner lift
      }, // end of outer anonymous match
      v1, mkErr("Int", v1)) // end of outer lift

    // Boolean operators  
    case "&&" | "||" => 
      lift(_ match{
        case BoolValue(b1) => 
          lift(_ match{
            case BoolValue(b2) => op match{
              case "&&" => BoolValue(b1 && b2); case "||" => BoolValue(b1 || b2)
            }
          },                        // end of inner anonymous match
          v2, mkErr("Boolean", v2)) // end of inner lift
      },                            // end of outer anonymous match
      v1, mkErr("Boolean", v1))     // end of outer lift
  }).withSource(v1.source until v2.source)
  // TODO: Give better error for division.

  // Note: the following is for testing only: it over-uses parentheses.
  override def toString = s"($left $op $right)"
}

// ==================================================================

/** A row literal. */
case class RowExp(row: Int) extends Exp{
  def eval(env: Environment) = RowValue(row).withSource(extent)

  override def toString = s"#$row"
}

// ==================================================================

/** A column literal. */
case class ColumnExp(column: String) extends Exp{
  require(column.forall(_.isUpper))
  require(column.length <= 2) // surely? 

  /** The Int that acts as the index for this column. */
  private val asInt: Int = 
    if(column.length == 1) column(0)-'A' 
    else (column(0)-'A'+1)*26 + column(1)-'A'

  def eval(env: Environment) = ColumnValue(asInt).withSource(extent)

  override def toString = s"#$column"
}

// ==================================================================

/** A reference to a Cell.  Note: the coordinates are in the order
  * (column,row), matching standard spreadsheet usage. */
case class CellExp(column: Exp, row: Exp) extends Exp{
  def eval(env: Environment) = {
    val cc = column.eval(env)
    lift(_ match{
      case ColumnValue(c) => 
        val rr = row.eval(env)
        lift(_ match{ case RowValue(r) => env.getCell(c, r) },
          rr, s"Expected row number, found $rr")
    }, 
    cc, s"Expected column identifier, found $cc")
  }

  override def toString = s"Cell($column, $row)"
}

// ==================================================================

object Exp{

}
