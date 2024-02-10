package spreadsheet

/** Representation of an expression. */
trait Exp{
  /** Evaluate this in environment `env`. */
  def eval(env: Environment): Value
}

// ==================================================================

/** A name. */
case class NameExp(name: String) extends Exp{
  def eval(env: Environment) = ???

  override def toString = name
}

// ==================================================================

/** An integer expression. */
case class IntExp(value: Int) extends Exp{
  def eval(env: Environment) = IntValue(value)

  override def toString = value.toString
}

// ==================================================================
 
/** An application of a binary operator. */
case class BinOp(left: Exp, op: String, right: Exp) extends Exp{
  def eval(env: Environment) = {
    doBinOp(left.eval(env), op, right.eval(env))
  }

  import Exp.{lift, mkErr}

  /** Apply the operation represented by `op` to values `v1` and `v2`. */
  private def doBinOp(v1: Value, op: String, v2: Value): Value = op match{
    case "+" | "-" | "*" | "/" | "==" | "!=" | "<" | "<=" | ">" | ">=" =>
      // (Int, Int) => Int operators 
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

    case "&&" | "||" => 
      lift(_ match{
        case BoolValue(b1) => 
          lift(_ match{
            case BoolValue(b2) => op match{
              case "&&" => BoolValue(b1 && b2); case "||" => BoolValue(b1 || b2)
            }
          },                                  // end of inner anonymous match
          v2, mkErr("Boolean", v2)) // end of inner lift
      },                                      // end of inner anonymous match
      v1, mkErr("Boolean", v1))     // end of outer lift
  }
  // TODO: Give better error for division.

  // Note: the following is for testing only: it over-uses parentheses.
  override def toString = s"($left $op $right)"
}

// ==================================================================

/** A row literal. */
case class RowExp(row: Int) extends Exp{
  def eval(env: Environment) = RowValue(row)

  override def toString = s"#$row"
}

/** A column literal. */
case class ColumnExp(column: String) extends Exp{
  require(column.forall(_.isUpper))
  require(column.length <= 2) // surely? 

  /** The Int that acts as the index for this column. */
  private val asInt: Int = 
    if(column.length == 1) column(0)-'A' 
    else (column(0)-'A'+1)*26 + column(1)-'A'

  def eval(env: Environment) = ColumnValue(asInt)

  override def toString = s"#$column"
}

// ==================================================================

/** A reference to a Cell.  Note: the coordinates are in the order
  * (column,row), matching standard spreadsheet usage. */
case class CellExp(column: Exp, row: Exp) extends Exp{
  import Exp.lift

  def eval(env: Environment) = {
    val cc = column.eval(env)
    // val ColumnValue(c) = column.eval(env)
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
  def mkErr(expected: String, found: Any) = 
    s"Expected $expected, found \"$found\""

  /** Extend f(v) to: (1) cases where v is an ErrorValue (passing on the error),
    * and (2) other types, returning a TypeError(err). */ 
  def lift(f: PartialFunction[Value, Value], v: Value, err: String)
      : Value = {
    if(f.isDefinedAt(v)) f(v) 
    else v match{ case ev: ErrorValue => ev; case _ => TypeError(err) }
  }
}
