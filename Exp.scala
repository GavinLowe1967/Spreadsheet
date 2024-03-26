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
  // protected def mkTypeError(expected: String, found: Value) = 
  //   TypeError(mkErr(expected, found))

  /** Extend f(v) to: cases where v is an ErrorValue (passing on the error).
    * Other cases shouldn't happen. */ 
  protected def lift(f: PartialFunction[Value, Value], v: Value)
      : Value = {
    if(f.isDefinedAt(v)) f(v) 
    else v match{ 
      case ev: ErrorValue => liftError(ev)
      case _ => sys.error(s"unexpected value: $v")
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

  /** Information about an overloaded binary operator: its representation is a
    * partial function, which (on application to the first argument) gives a
    * partial function and the expected type of the second argument. */
  // type BinOpInfo = PF[(PF[Value], TypeT)] // 

  // type BinOpInfoX = (PF[PF[Value]], TypeT, TypeT)

  // /** Make a function (IntType,IntType) -> Value or (FloatType,FloatType) ->
  //   * Value by lifting fi and ff. */
  // private def mkBinOp(fi: (Int,Int) => Value, ff: (Float,Float) => Value)
  //     : BinOpInfo = {
  //   case IntValue(n1) =>
  //     ({ case IntValue(n2) => fi(n1,n2) }, IntType)
  //   case FloatValue(x1) =>
  //     ({ case FloatValue(x2) => ff(x1,x2) }, FloatType)
  // }

  // /** Specialise to (Num,Num) -> Num functions. */
  // private def mkBinNumOp(fi: (Int,Int) => Int, ff: (Float,Float) => Float)
  //     : BinOpInfo =
  //   mkBinOp({case (n1,n2) => IntValue(fi(n1,n2))}, 
  //           {case (x1,x2) => FloatValue(ff(x1,x2))} )

  // /** Specialise to (Num,Num) -> Bool functions. */
  // private def mkBinRelOp(fi: (Int,Int) => Boolean, ff: (Float,Float) => Boolean)
  //     : BinOpInfo =
  //   mkBinOp({case (n1,n2) => BoolValue(fi(n1,n2))}, 
  //           {case (x1,x2) => BoolValue(ff(x1,x2))} )


  // /** Information about the "==" (if `eq`) or "!=" (otherwise) operator.
  //   * Returns a pair `(f,ts)` where (a) `f` is a partial function (over the
  //   * first argument) that returns a pair `(f2,t2)`, where `f2` is the
  //   * resulting partial function on the second argument, and `t2` is the
  //   * expected type of the second argument; (b) `ts` is the list of types
  //   * accepted for the first argument. */
  // private def equalOp(eq: Boolean): (PF[(PF[Value], TypeT)], List[TypeT]) = (
  //   {
  //     case IntValue(n1) =>
  //       ({ case IntValue(n2) => BoolValue((n1 == n2) == eq ) }, IntType)
  //     case BoolValue(b1) =>
  //       ({ case BoolValue(b2) => BoolValue((b1 == b2) == eq) }, BoolType)
  //     case l1 : ListValue => 
  //       ({ case l2 : ListValue if l1.underlying.comparable(l2.underlying) =>
  //            BoolValue((l1.elems == l2.elems) == eq) }, 
  //         ListType(l1.underlying))
  //       // Note: we allow equality tests only between lists with comparable
  //       // underlying types.  So we allow "tail([1]) == []", but not
  //       // "tail([1]) == tail([false])"
  //   },
  //   List(IntType, BoolType, ListType(AnyType))
  // )

  // /** Make information about an (Boolean,Boolean) => Boolean binary relation
  //   * operator corresponding to f. */
  // private def mkBoolBinOp(f: (Boolean,Boolean) => Boolean): BinOpInfoX = (
  //   { case BoolValue(b1) => { case BoolValue(b2) => BoolValue(f(b1,b2)) } },
  //   BoolType, BoolType
  // )

  // /** Apply the operation represented by `op` to values `v1` and `v2`. */
  // private def doBinOpX(v1: Value, op: String, v2: Value): Value = {
  //   // Create an ErrorValue, corresponding to `v` being found when an argument
  //   // of type described by `typeString` was expected.
  //   def mkError0(v: Value, typeString: String) = v match{
  //     case ev: ErrorValue => liftError(ev);
  //     case _ => sys.error(mkErr(typeString, v))
  //   }
  //   def mkError(v: Value, t: TypeT) = mkError0(v, t.asString)
  //   def mkErrorX(v: Value, ts: List[TypeT]) =
  //     mkError0(v, ts.map(_.asString).mkString(" or "))

  //   if(op == "==" || op == "!="){ // overloaded function
  //     val (f,ts) = equalOp(op == "==")
  //     if(f.isDefinedAt(v1)){
  //       val (f1,t2) = f(v1); if(f1.isDefinedAt(v2)) f1(v2) else mkError(v2, t2)
  //     }
  //     else mkErrorX(v1, ts)
  //   }
  //   else if(List("+", "-", "*", "/", "<=", "<", ">", ">=").contains(op)){ 
  //     val f : PF[(PF[Value], TypeT)] = op match{
  //       case "+" => mkBinNumOp((_+_), (_+_))
  //       case "-" => mkBinNumOp((_-_), (_-_))
  //       case "*" => mkBinNumOp((_*_), (_*_)) 
  //       case "/" => 
  //         def err = liftError(EvalError("Division by zero"))
  //         mkBinOp(
  //           {case (n1,n2) => if(n2 != 0) IntValue(n1/n2) else err},
  //           {case (x1,x2) => if(x2 != 0.0) FloatValue(x1/x2) else err})
  //       case "<=" => mkBinRelOp((_<=_), (_<=_))
  //       case "<" => mkBinRelOp((_<_), (_<_))
  //       case ">=" => mkBinRelOp((_>=_), (_>=_))
  //       case ">" => mkBinRelOp((_>_), (_>_))
  //     }
  //     if(f.isDefinedAt(v1)){
  //       val (f1,t2) = f(v1)
  //       if(f1.isDefinedAt(v2)) f1(v2) else mkError(v2,t2) 
  //     }
  //     else mkErrorX(v1, List(IntType, FloatType)) 
  //   }
  //   else{
  //     val (f,t1,t2) = op match{
  //       case "||" => mkBoolBinOp(_||_); case "&&" => mkBoolBinOp(_&&_)
  //     }
  //     if(f.isDefinedAt(v1)){
  //       val f1 = f(v1); if(f1.isDefinedAt(v2)) f1(v2) else mkError(v2, t2)
  //     }
  //     else mkError(v1,t1)
  //   }
  // }

  private type BinOpRep = PF[PF[Value]]

  /** (Bool,Bool) -> Bool functions. */
  private def mkBoolOp1(f: (Boolean,Boolean) => Boolean): BinOpRep = {
    case BoolValue(b1) => { case BoolValue(b2) => BoolValue(f(b1,b2)) }
  }

  /** (Num, Num) -> Value funcitons. */
  private def mkBinOp1(fi: (Int,Int) => Value, ff: (Float,Float) => Value) 
      : BinOpRep = {
    case IntValue(n1) => { case IntValue(n2) => fi(n1,n2) }
    case FloatValue(x1) => { case FloatValue(x2) => ff(x1,x2) }
  }

  /** (Num, Num) -> Value functions. */
  private def mkBinNumOp1(fi: (Int,Int) => Int, ff: (Float,Float) => Float)
      : BinOpRep =
    mkBinOp1({case (n1,n2) => IntValue(fi(n1,n2))},
             {case (x1,x2) => FloatValue(ff(x1,x2))} )

  private def mkBinRelOp1(fi: (Int,Int) => Boolean, ff: (Float,Float) => Boolean)
      : BinOpRep =
    mkBinOp1({case (n1,n2) => BoolValue(fi(n1,n2))}, 
             {case (x1,x2) => BoolValue(ff(x1,x2))} )

  private def equalOp1(eq: Boolean): BinOpRep = {
    def mkRes(b: Boolean) = BoolValue(b == eq)
    _ match {
      case IntValue(n1) => { case IntValue(n2) => mkRes(n1 == n2) }
      case FloatValue(x1) => { case FloatValue(x2) => mkRes(x1 == x2) }
      case BoolValue(b1) => { case BoolValue(b2) => mkRes(b1 == b2) }
    // FIXME: complete
    }
  }

  private def doBinOp(v1: Value, op: String, v2: Value): Value = {
    val f : BinOpRep = op match{
      case "+" => mkBinNumOp1((_+_), (_+_))
      case "-" => mkBinNumOp1((_-_), (_-_))
      case "*" => mkBinNumOp1((_*_), (_*_))
      case "/" => 
        def err = liftError(EvalError("Division by zero"))
        mkBinOp1(
          {case (n1,n2) => if(n2 != 0) IntValue(n1/n2) else err},
          {case (x1,x2) => if(x2 != 0.0) FloatValue(x1/x2) else err})
      case "<=" => mkBinRelOp1((_<=_), (_<=_))
      case "<" => mkBinRelOp1((_<_), (_<_))
      case ">=" => mkBinRelOp1((_>=_), (_>=_))
      case ">" => mkBinRelOp1((_>_), (_>_))
      case "&&" => mkBoolOp1((_&&_))
      case "||" => mkBoolOp1((_||_))
      case "==" => equalOp1(true)
      case "!=" => equalOp1(false)
    }
    if(f.isDefinedAt(v1)){ val f1 = f(v1); lift(f1, v2) }
    else v1 match{
      case ev: ErrorValue => liftError(ev) 
      case _ => sys.error(s"unexpected value: $v1")
    }
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
    var theType: TypeT = AnyType
    while(es.nonEmpty && error == null){
      es.head.eval(env) match{
        case err: ErrorValue => error = liftError(err)
        case v => 
          if(v.isOfType(theType)){ vs ::= v; theType = v.getType; es = es.tail }
          else sys.error(mkErr(theType.asString, v))
      }
    }
    if(error != null) error else ListValue(theType, vs.reverse)
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
