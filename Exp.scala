package spreadsheet

/** The result of a parse with an Extent. */
trait HasExtent{
  /** The Extent representing the string from which this was produced. */
  protected var extent: Extent = null

  def getExtent = extent

  /** Set the Extent representing the string from which this was produced. */
  def setExtent(e: Extent) = extent = e  

  protected val tab = "     "

  /** Lift an error value, by tagging on the extent of this. */
  def liftError(error: ErrorValue) = {
    assert(extent != null, s"Null extent in $this")
    error match{
      case TypeError(msg) => TypeError(s"$msg\n${tab}in \"${extent.asString}\"")
      case EvalError(msg) => EvalError(s"$msg\n${tab}in \"${extent.asString}\"")
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

  /** Evaluate this in environment `env`, but if it doesn't produce a result of
    * type `t`, return a type error. */
  def evalExpectType(env: Environment, t: TypeT): Value = {
    eval(env) match{
      case err: ErrorValue => err
      case v => 
        if(v.isOfType(t)) v
        else liftError(
          TypeError(s"Expected ${t.asString}, found value ${v.forError}"))
    }
  }

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

  /** Make a TypeError, that `found` was found when `expected` was expected`. */
  protected def mkTypeError(expected: String, found: Value) = 
    TypeError(mkErr(expected, found))

  /** Extend f(v) to: (1) cases where v is an ErrorValue (passing on the error),
    * and (2) other types, returning a TypeError(err). */ 
  protected def lift(f: PartialFunction[Value, Value], v: Value, err: String)
      : Value = {
    if(f.isDefinedAt(v)) f(v) 
    else v match{ 
      case ev: ErrorValue => liftError(ev); 
      case _ => TypeError(err).withSource(extent)
    }
  }
}

// ==================================================================

/** A name. */
case class NameExp(name: NameExp.Name) extends Exp{
  def eval0(env: Environment) = {
    assert(extent != null, s"Null extent in $this")
    env.get(name) match{
      case Some(value) => value 
      case None => EvalError(s"Name not found: $name")
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

  /** Information about a non-overloaded binary operator: its representation as
    * a curried partial function, and the types of its two arguments. */
  type BinOpInfo = (PF[PF[Value]], TypeT, TypeT)

  /** Make information about an (Int,Int) => Int binary operator corresponding
    * to f. */
  private def mkIntBinOp(f: (Int,Int) => Int): BinOpInfo = (
    { case IntValue(n1) => { case IntValue(n2) => IntValue(f(n1,n2)) } },
    IntType, IntType
  )

  /** Information for the "/" binary operator. */
  private def div: BinOpInfo = ({
    case IntValue(n1) => { 
      case IntValue(n2) =>
        if(n2 == 0) liftError(EvalError("Division by zero")) // IMPROVE?
        else IntValue(n1/n2) 
    }},
    IntType, IntType
  )

  /** Make information about an (Int,Int) => Boolean binary relation operator
    * corresponding to f. */
  private def mkIntRel(f: (Int,Int) => Boolean): BinOpInfo  = (
    { case IntValue(n1) => { case IntValue(n2) => BoolValue(f(n1,n2)) } },
    IntType, IntType
  )

  /** Information about the "==" (if `eq`) or "!=" (otherwise) operator.
    * Returns a pair `(f,ts)` where (a) `f` is a partial function (over the
    * first argument) that returns a pair `(f2,t2)`, where `f2` is the
    * resulting partial function on the second argument, and `t2` is the
    * expected type of the second argument; (b) `ts` is the list of types
    * accepted for the first argument. */
  private def equalOp(eq: Boolean): (PF[(PF[Value], TypeT)], List[TypeT]) = (
    {
      case IntValue(n1) =>
        ({ case IntValue(n2) => BoolValue((n1 == n2) == eq ) }, IntType)
      case BoolValue(b1) =>
        ({ case BoolValue(b2) => BoolValue((b1 == b2) == eq) }, BoolType)
      case l1 : ListValue => 
        ({ case l2 : ListValue if l1.underlying.comparable(l2.underlying) =>
             BoolValue((l1.elems == l2.elems) == eq) }, 
          ListType(l1.underlying))
        // Note: we allow equality tests only between lists with comparable
        // underlying types.  So we allow "tail([1]) == []", but not
        // "tail([1]) == tail([false])"
    },
    List(IntType, BoolType, ListType(AnyType))
  )

  /** Make information about an (Boolean,Boolean) => Boolean binary relation
    * operator corresponding to f. */
  private def mkBoolBinOp(f: (Boolean,Boolean) => Boolean): BinOpInfo = (
    { case BoolValue(b1) => { case BoolValue(b2) => BoolValue(f(b1,b2)) } },
    BoolType, BoolType
  )

  /** Apply the operation represented by `op` to values `v1` and `v2`. */
  private def doBinOp(v1: Value, op: String, v2: Value): Value = {
    // Create an ErrorValue, corresponding to `v` being found when an argument
    // of type described by `typeString` was expected.
    def mkError0(v: Value, typeString: String) = v match{
      case ev: ErrorValue => liftError(ev);
      case _ => TypeError(mkErr(typeString, v)).withSource(extent)
    }
    def mkError(v: Value, t: TypeT) = mkError0(v, t.asString)
    def mkErrorX(v: Value, ts: List[TypeT]) =
      mkError0(v, ts.map(_.asString).mkString(" or "))

    if(op == "==" || op == "!="){ // overloaded function
      val (f,ts) = equalOp(op == "==")
      if(f.isDefinedAt(v1)){
        val (f1,t2) = f(v1); if(f1.isDefinedAt(v2)) f1(v2) else mkError(v2, t2)
      }
      else mkErrorX(v1, ts)
    }
    else{
      val (f,t1,t2) = op match{
        case "+" => mkIntBinOp(_+_ ); case "-" => mkIntBinOp(_-_)
        case "*" => mkIntBinOp(_*_); case "/" => div
        // case "==" => mkIntRel(_==_); case "!=" => mkIntRel(_!=_)
        case "<=" => mkIntRel(_<=_); case "<" => mkIntRel(_<_);
        case ">" => mkIntRel(_>_); case ">=" => mkIntRel(_>=_)
        case "||" => mkBoolBinOp(_||_); case "&&" => mkBoolBinOp(_&&_)
      }
      if(f.isDefinedAt(v1)){
        val f1 = f(v1); if(f1.isDefinedAt(v2)) f1(v2) else mkError(v2, t2)
      }
      else mkError(v1,t1)
    }
  }

  /** Apply the operation represented by `op` to values `v1` and `v2`. */
  // private def doBinOpX(v1: Value, op: String, v2: Value): Value = (op match{
  //   // (Int, Int) operators
  //   case "+" | "-" | "*" | "/" | "==" | "!=" | "<" | "<=" | ">" | ">=" =>
  //     lift(_ match{
  //       case IntValue(n1) => 
  //         lift(_ match{
  //           case IntValue(n2) => op match{
  //             case "+" => IntValue(n1+n2); case "-" => IntValue(n1-n2)
  //             case "*" => IntValue(n1*n2)
  //             case "/" =>
  //               if(n2 == 0) liftError(EvalError("Division by zero"))
  //               else IntValue(n1/n2)
  //             case "==" => BoolValue(n1 == n2); case "!=" => BoolValue(n1 != n2)
  //             case "<" => BoolValue(n1 < n2); case "<=" => BoolValue(n1 <= n2)
  //             case ">" => BoolValue(n1 > n2); case ">=" => BoolValue(n1 >= n2)
  //           } // end of op match
  //         }, // end of anonymous match
  //         v2, mkErr("Int", v2)) // end of inner lift
  //     }, // end of outer anonymous match
  //     v1, mkErr("Int", v1)) // end of outer lift

  //   // Boolean operators  
  //   case "&&" | "||" => 
  //     lift(_ match{
  //       case BoolValue(b1) => 
  //         lift(_ match{
  //           case BoolValue(b2) => op match{
  //             case "&&" => BoolValue(b1 && b2); case "||" => BoolValue(b1 || b2)
  //           }
  //         },                        // end of inner anonymous match
  //         v2, mkErr("Boolean", v2)) // end of inner lift
  //     },                            // end of outer anonymous match
  //     v1, mkErr("Boolean", v1))     // end of outer lift
  // }).withSource(v1.source until v2.source)

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

  /** The Int that acts as the index for this column. */
  private val asInt: Int = 
    if(column.length == 1) column(0)-'A' 
    else (column(0)-'A'+1)*26 + column(1)-'A'

  def eval0(env: Environment) = ColumnValue(asInt)

  override def toString = s"#$column"
}

// ==================================================================

/** A reference to a Cell.  Note: the coordinates are in the order
  * (column,row), matching standard spreadsheet usage. */
case class CellExp(column: Exp, row: Exp) extends Exp{
  def eval0(env: Environment) = ??? 
  // Note: above never called, since env.getCell sets the source

  override def eval(env: Environment) = {
    val cc = column.eval(env)
    lift(_ match{
      case ColumnValue(c) => 
        val rr = row.eval(env)
        lift(_ match{ case RowValue(r) => env.getCell(c, r) },
          rr, s"Expected row number, found ${rr.forError}")
    }, 
    cc, s"Expected column identifier, found ${cc.forError}")
  }

  override def toString = s"Cell($column, $row)"
}

// ==================================================================

/** An if expression `if(test) thenClause else elseClause`. */
case class IfExp(test: Exp, thenClause: Exp, elseClause: Exp) extends Exp{
  def eval0(env: Environment) = test.eval(env) match{
    case BoolValue(true) => thenClause.eval(env)
    case BoolValue(false) => elseClause.eval(env)
    case err: ErrorValue => liftError(err)
    case other => mkTypeError("Boolean", other)
  }

}

// =================================================================

case class ListLiteral(elems: List[Exp]) extends Exp{
  def eval0(env: Environment) = {
    // Traverse elems, evaluating each, and building in vs in reverse;
    // maintain type of elements in theType; and catch any error.
    var es = elems; var vs = List[Value](); var error: ErrorValue = null
    var theType: TypeT = AnyType
    while(es.nonEmpty && error == null){
      es.head.eval(env) match{
        case err: ErrorValue => error = liftError(err)
        case v => 
          if(v.isOfType(theType)){ vs ::= v; theType = v.getType; es = es.tail }
          else error = mkTypeError(theType.asString, v)
      }
    }
    if(error != null) error else ListValue(theType, vs.reverse)
  }
}

// ==================================================================

object Exp{

}

// ========= Note =========
// FunctionValue.scala contains another subclass, FunctionApp, and 
// BlockExp.scala contains another subclass, BlockExp.
