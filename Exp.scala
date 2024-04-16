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

  // import BinOp._

  // /** Apply the operation represented by `op` to values `v1` and `v2`. */
  // def doBinOp(v1: Value, v2: => Value): Value = {
  //   // The representation of op as a BinOpRep
  //   val f : BinOpRep = op match{
  //     case "+" => mkBinNumOp((_+_), (_+_)); case "-" => mkBinNumOp((_-_), (_-_))
  //     case "*" => mkBinNumOp((_*_), (_*_))
  //     case "/" => 
  //       def err = liftError(EvalError("Division by zero"), true) // inc line num
  //       mkBinOp({case (n1,n2) => if(n2 != 0) IntValue(n1/n2) else err},
  //         {case (x1,x2) => if(x2 != 0.0) FloatValue(x1/x2) else err} )
  //     case "<=" => mkBinRelOp((_<=_), (_<=_))
  //     case "<" => mkBinRelOp((_<_), (_<_))
  //     case ">=" => mkBinRelOp((_>=_), (_>=_))
  //     case ">" => mkBinRelOp((_>_), (_>_))
  //     case "&&" => mkBoolOp((_&&_)); case "||" => mkBoolOp((_||_))
  //     case "==" => equalOp(true); case "!=" => equalOp(false)
  //     case "::" => consOp
  //   }
  //   if(f.isDefinedAt(v1)){ val f1 = f(v1); lift(f1, v2) }
  //   else handleError(v1)
  // }

  // Note: the following is for testing only: it over-uses parentheses.
  override def toString = s"($left $op $right)"
}

// =================================

// /** Companion object for BinOp, giving semantics to the operators. */
// object BinOp{

//   /** Apply the operation represented by `op` to values `v1` and `v2`. 
//     * Pre: v1 and v2 are non-error values, so applying op makes sense. */
//   def doBinOp(v1: Value, op: String, v2: => Value): Value = {
//     // The representation of op as a BinOpRep
//     val f : BinOpRep = op match{
//       case "+" => mkBinNumOp((_+_), (_+_)); case "-" => mkBinNumOp((_-_), (_-_))
//       case "*" => mkBinNumOp((_*_), (_*_))
//       case "/" => 
//         def err = EvalError("Division by zero")
//         mkBinOp({case (n1,n2) => if(n2 != 0) IntValue(n1/n2) else err},
//           {case (x1,x2) => if(x2 != 0.0) FloatValue(x1/x2) else err} )
//       case "<=" => mkBinRelOp((_<=_), (_<=_))
//       case "<" => mkBinRelOp((_<_), (_<_))
//       case ">=" => mkBinRelOp((_>=_), (_>=_))
//       case ">" => mkBinRelOp((_>_), (_>_))
//       case "&&" => mkBoolOp((_&&_)); case "||" => mkBoolOp((_||_))
//       case "==" => equalOp(true); case "!=" => equalOp(false)
//       case "::" => consOp
//     }    
//     f(v1)(v2)
//   }

//   /** Shorthand for a partial function Value => A. */
//   type PF[A] = PartialFunction[Value, A]

//   /** Representation of a binary operator as a curried partial function. */
//   private type BinOpRep = PF[PF[Value]]

//   /* The functions below define BinOpReps by lifting functions over the
//    * primitive types. */

//   /** (Bool,Bool) -> Bool functions. */
//   private def mkBoolOp(f: (Boolean,Boolean) => Boolean): BinOpRep = {
//     case BoolValue(b1) => { case BoolValue(b2) => BoolValue(f(b1,b2)) }
//   }

//   /** (Num, Num) -> Value functions. */
//   private def mkBinOp(fi: (Int,Int) => Value, ff: (Float,Float) => Value) 
//       : BinOpRep = {
//     case IntValue(n1) => { case IntValue(n2) => fi(n1,n2) }
//     case FloatValue(x1) => { case FloatValue(x2) => ff(x1,x2) }
//   }

//   /** (Num, Num) -> Num functions. */
//   private def mkBinNumOp(fi: (Int,Int) => Int, ff: (Float,Float) => Float)
//       : BinOpRep =
//     mkBinOp({case (n1,n2) => IntValue(fi(n1,n2))},
//             {case (x1,x2) => FloatValue(ff(x1,x2))} )

//   /** (Num, Num) -> Bool functions. */
//   private def mkBinRelOp(fi: (Int,Int) => Boolean, ff: (Float,Float) => Boolean)
//       : BinOpRep =
//     mkBinOp({case (n1,n2) => BoolValue(fi(n1,n2))}, 
//             {case (x1,x2) => BoolValue(ff(x1,x2))} )

//   /** Equality operators.
//     * @param eq Is this representing the equality operator (==), as opposed to
//     * inequality (!=)? */
//   private def equalOp(eq: Boolean): BinOpRep = {
//     // Build the result from b which represents equality. 
//     def mkRes(b: Boolean) = BoolValue(b == eq)
//     _ match {
//       case IntValue(n1) => { case IntValue(n2) => mkRes(n1 == n2) }
//       case FloatValue(x1) => { case FloatValue(x2) => mkRes(x1 == x2) }
//       case BoolValue(b1) => { case BoolValue(b2) => mkRes(b1 == b2) }
//       case StringValue(st1) => { case StringValue(st2) => mkRes(st1 == st2) }
//       case RowValue(r1) => { case RowValue(r2) => mkRes(r1 == r2) }
//       case ColumnValue(c1) => { case ColumnValue(c2) => mkRes(c1 == c2) }
//       case ListValue(elems1) => { case ListValue(elems2) => 
//         mkRes(elems1 == elems2) } // Note: lists can contain no error values
//     }
//   }

//   /** Representation of the cons (::) operator. */
//   private def consOp: BinOpRep = {
//     case (v: Value) => { case ListValue(vs) => ListValue(v::vs) }
//   }
// }


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
  * (column,row), matching standard spreadsheet usage. */
case class CellExp(column: Exp, row: Exp) extends Exp{

  /** The type associated with this read of a cell.  It might be a TypeVar, in
    * which case the corresponding TypeEnv will have a constraint upon it. */
  var theType: TypeT = null

  def setType(t: TypeT) = theType = t 

  override def toString = s"Cell($column, $row)"
}

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
