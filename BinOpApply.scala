package spreadsheet

/** Object giving semantics to the binary infix operators. */
object BinOpApply{
  /** Apply the operation represented by `op` to values `v1` and `v2`. 
    * Pre: v1 and v2 are non-error values, so applying op makes sense. */
  def apply(v1: Value, op: String, v2: => Value): Value = {
    // The representation of op as a BinOpRep
    val f : BinOpRep = op match{
      case "+" => mkArith((_+_)); case "-" => mkArith((_-_))
      case "*" => mkBinNumOp((_*_), (_*_))
      case "/" => 
        def err = EvalError("Division by zero")
        mkBinOp({case (n1,n2) => if(n2 != 0) IntValue(n1/n2) else err},
          {case (x1,x2) => if(x2 != 0.0) FloatValue(x1/x2) else err} )
      case "<=" => mkBinRelOp((_<=_), (_<=_))
      case "<" => mkBinRelOp((_<_), (_<_))
      case ">=" => mkBinRelOp((_>=_), (_>=_))
      case ">" => mkBinRelOp((_>_), (_>_))
      case "&&" => mkBoolOp((_&&_)); case "||" => mkBoolOp((_||_))
      case "==" => equalOp(true); case "!=" => equalOp(false)
      case "::" => consOp
      case "to" => toOp; case "until" => untilOp
    }    
    f(v1)(v2)
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
    case IntValue(n1) => { 
      case IntValue(n2) => fi(n1,n2) 
      case FloatValue(x2) => ff(n1.toFloat, x2)
    }
    case FloatValue(x1) => { 
      case FloatValue(x2) => ff(x1,x2) 
      case IntValue(n2) => ff(x1, n2.toFloat)
    }
  }

  /** (Num, Num) -> Num functions. */
  private def mkBinNumOp(fi: (Int,Int) => Int, ff: (Float,Float) => Float)
      : BinOpRep =
    mkBinOp({case (n1,n2) => IntValue(fi(n1,n2))},
            {case (x1,x2) => FloatValue(ff(x1,x2))} )

  /** Functions over Arith. */
  private def mkArith(f: (Arith,Arith) => Value): BinOpRep = {
    case a1: Arith => { case a2: Arith => f(a1,a2) }
  }

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
      case IntValue(n1) => { 
        case IntValue(n2) => mkRes(n1 == n2) 
        case FloatValue(x2) => mkRes(n1.toFloat == x2)
      }
      case FloatValue(x1) => { 
        case FloatValue(x2) => mkRes(x1 == x2) 
        case IntValue(n2) => mkRes(x1 == n2.toFloat)
      }
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
    case (v: Value) => { case ListValue(vs) => ListValue(v::vs) }
  }

  /** Representation of the "to" operation. */
  private val toOp: BinOpRep = {
    case v1: Rangeable => { case v2: Rangeable => v1 to v2 }
  }

  /** Representation of the "until" operation. */
  private val untilOp: BinOpRep = {
    case v1: Rangeable => { case v2: Rangeable => v1 until v2 }
  }
}
