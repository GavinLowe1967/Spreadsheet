package spreadsheet

import FunctionDeclaration.ParameterList
import scala.collection.mutable.ArrayBuffer

/** Object responsible for evaluating expressions and executing statements. */
object Execution{
  // ===== Helper functions

  /** Lift an error value, by tagging on the extent of e. 
    * @param lineNum if true, include line number. */
  private 
  def liftError(e: HasExtent, error: ErrorValue, lineNum: Boolean = false)
      : ErrorValue = {
    val extent = e.getExtent; assert(extent != null, s"Null extent in $e")
    val lnString = if(lineNum) " at line "+extent.lineNumber else ""
    def extend(msg: String) = s"$msg$lnString\nin \"${extent.asString}\""
    error match{
      case TypeError(msg) => TypeError(extend(msg))
      case EvalError(msg) => EvalError(extend(msg))
      case m @ MultipleWriteError(sources) => m
        // Note: this isn't quite right, as we'd like to lift the relevant
        // element of sources
    } 
  }

  /** Lift an EvalError value, but not other types of error, by tagging on the
    * extent of e.
    * @param lineNum if true, include line number. */
  private 
  def maybeLiftError(e: HasExtent, error: ErrorValue, lineNum: Boolean = false)
      : ErrorValue = {
    val extent = e.getExtent; assert(extent != null, s"Null extent in $e")
    val lnString = if(lineNum) " at line "+extent.lineNumber else ""
    def extend(msg: String) = s"$msg$lnString\nin \"${extent.asString}\""
    error match{
      case te: TypeError => te
      case EvalError(msg) => EvalError(extend(msg))
      case m: MultipleWriteError => m
      //case m @ MultipleWriteError(sources) => m
        // Note: this isn't quite right, as we'd like to lift the relevant
        // element of sources
    } 
  }

  /** Extend f(v) to: cases where v is an ErrorValue (passing on the error,
    * lifting to e).  Other cases shouldn't happen. */ 
  private def lift(e: Exp)(f: PartialFunction[Value, Value], v: Value) : Value = 
    if(f.isDefinedAt(v)) f(v) 
    else v match{
      case ev: ErrorValue => liftError(e, ev)
      case _ => sys.error(s"unexpected value: $v")
    } //e.handleError(v)

  /** If v is an error, lift it by tagging it with the extent of e. */
  private def liftValue(e: Exp, v: Value, lineNum: Boolean = false)
      : Value = v match{
    case err: ErrorValue => liftError(e, err, lineNum)
    case _ => v
  }

  /** Check value `v` read from cell `name` has type `eType`.  If so, return
    * `v`.  If not, give appropriate TypeError.  */
  private def checkCellType(eType: TypeT)(v: Cell, name: String) = {
    assert(eType != null && v != null && name != null && v.getType != null,
      s"checkCellType($eType)($v, $name)")
    val vType = v.getType
    if(vType == eType) v
    else TypeError(
      s"Expected ${eType.asString}, found ${vType.asString} in cell "+name)
  }

  // ===== Evaluation

  /** Evaluate `e` in environment `env`, adding the extent from `e`. */
  private def eval(env: Environment, e: Exp): Value = {
    /* If column and row evaluate to a ColumnValue and RowValue, respectively,
     * apply comp to the cell's contents and the cell's name; otherwise give
     * an error. */
    def applyToCell(column: Exp, row: Exp, comp: (Cell,String) => Value)
        : Value = 
      lift(e)( { 
        case ColumnValue(c) =>
          lift(e)( 
            {case RowValue(r) =>
              comp(env.getCell(c,r), "#"+ColumnValue.getName(c)+r)},
            eval(env, row)
          ) },
        eval(env, column)
      ) // end of applyToCell

    e match{
      case CellExp(column, row, theType) =>
        // Check contents of cell has type theType
        val v1 = applyToCell(column, row, checkCellType(theType))
        liftValue(e, v1, true)

      case CellMatchExp(column, row, branches) =>
        // Try to match v against branches
        def doMatch(v: Cell, name: String) = {
          val vType = v.getType
          val bs = branches.filter(_.pattern.matches(vType))
          if(bs.isEmpty) 
            liftError(e, TypeError(
              s"Cannot match ${vType.asString} in cell $name"), true)
          else{ 
            val branch = bs.head
            val env1 = env.clone // clone env to prevent leakage
            bindMatch(env1, branch.pattern, v); eval(env1, branch.body)
          }
        }
        applyToCell(column, row, doMatch)

      case uce @ UntypedCellExp(column, row) => 
        // Check contents of cell has type uce.getType
        val v1 = applyToCell(column, row, checkCellType(uce.getType))
        liftValue(e, v1, true)

      case _ => eval0(env, e)
    }
  }

  /** Evaluate `e` in environment `env`. */
  private def eval0(env: Environment, e: Exp): Value = e match{
    case ne @ NameExp(name) => env(ne.getName) 
    case IntExp(value) => IntValue(value)
    case FloatExp(value) => FloatValue(value) 
    case BoolExp(value) => BoolValue(value)
    case StringExp(value) => StringValue(value)
    case UnitExp => UnitValue
    case RowExp(row) => RowValue(row)
    case c @ ColumnExp(column) => ColumnValue(c.asInt)

    case b @ BinOp(left, op, right) => eval(env, left) match{
      case err1: ErrorValue => liftError(e, err1)
      case v1 =>
        if(BinOpApply.isLazy(op))
          BinOpApply.lazyEval(v1, op, eval(env, right)) match{
            case err: ErrorValue => liftError(e, err); case v => v
              // Don't include line number in former case: it was an error in
              // right, so the line number is already included.
          }
        else eval(env, right) match{
          case err2: ErrorValue => liftError(e, err2)
          case v2 => BinOpApply(v1, op, v2) match{
            case err: ErrorValue => liftError(e, err, true)
            // Include line number, because this error results from the
            // operator itself (e.g. division by 0).
            case res => res
          }
        }
      }

    case IfExp(test, thenClause, elseClause) => 
      eval(env, test) match{
        case BoolValue(true) => eval(env, thenClause)
        case BoolValue(false) => eval(env, elseClause)
        case err: ErrorValue => liftError(e, err)
        case other => sys.error(s"Unexpected type: $other")
      }

    case ListLiteral(elems) => 
      evalList(env, elems) match{
        case Left(vs) => ListValue(vs); case Right(err) => liftError(e, err)
      }

    case ListComprehension(e1, qs) =>
      val ab = new ArrayBuffer[Value]
      val error = processListComp(env, e1, qs, ab)
      if(error != null) liftError(e, error) else ListValue(ab.toList)

    case TupleLiteral(es) => 
      evalList(env, es) match{
        case Left(vs) => TupleValue(vs); case Right(err) => liftError(e, err)
      }

    case fa @ FunctionApp(f, args) => eval(env, f) match{
      case fv : FunctionValue =>
        evalList(env, args) match{
          case Left(vs) => fv(vs) match{
            case err: ErrorValue => maybeLiftError(e, err, true)
              // Don't lift TypeErrors here, as that's confusing.  But include
              // line number for function call.
            case result => result
          }
          case Right(err) => liftError(e, err)
        }
      case err: ErrorValue => liftError(e, err)
      case other => sys.error(s"$f -> $other")
        // Note: eval0 is never called on a CellExpr
      } // end of case FunctionApp(...)

    case BlockExp(stmts, exp) => 
      val env1 = env.clone
      // If an error arises in performing stmts, it will be put in err.
      var err: ErrorValue = null
      def handleError(ev: ErrorValue) = { err = ev }
      performAll(stmts, env1, handleError)
      if(err == null){
        if(exp != null)
          eval(env1, exp) match{
            case ev: ErrorValue => liftError(e, ev); case res => res
          }
          else UnitValue
      }
      else liftError(e, err)

    case TypedExp(e, _) => eval(env, e)
  } // end of eval

  /** The result of evaluating exps if all succeed; or a relevant ErrorValue. */
  private def evalList(env: Environment, exps: List[Exp])
      : Either[List[Value], ErrorValue] = {
    // Traverse exps, evaluating each, and building in vs in reverse;
    // maintain type of elements in theType; and catch any error.
    var es = exps; var vs = List[Value](); var error: ErrorValue = null
    while(es.nonEmpty && error == null) eval(env, es.head) match{
      case err: ErrorValue => error = err; case v => vs ::= v; es = es.tail
    }
    if(error != null) Right(error) else Left(vs.reverse)
  }

  /** Evaluate a list comprehension with term e and qualifiers qs.  Add
    * resulting values to ab if there are no errors.  Return an error if there
    * is one; otherwise return null. */
  private def processListComp(
    env: Environment, e: Exp, qs: List[Qualifier], ab: ArrayBuffer[Value])
      : ErrorValue =
    if(qs.isEmpty) eval(env, e) match{
      case ev: ErrorValue => ev; case v => ab += v; null
    }
    else qs.head match{
      case Generator(pattern, list) =>
        eval(env, list) match{
          case ListValue(vs) => 
            // bind name to each element of vs; recurse; and combine results.
            var vs1 = vs; var error: ErrorValue = null
            while(vs1.nonEmpty && error == null){
              val env1 = env.clone; bind(env1, pattern, vs1.head); vs1 = vs1.tail
              error = processListComp(env1, e, qs.tail, ab)
            }
            error
          case err: ErrorValue => err
        }
      case Filter(test) => 
        eval(env, test) match{
          case BoolValue(b) => 
            if(b) processListComp(env, e, qs.tail, ab) else null
          case err: ErrorValue => err
        }
    }

  /** Update env by performing the binding corresponding to v matching cell. */
  private 
  def bindMatch(env: Environment, pat: MatchPattern, v: Cell): Unit = pat match{
    case TypedPattern(Some(name), _) => env.update(name, v)
    case _ => {}
  }

  /** Update env, binding pat to v. */
  private def bind(env: Environment, pat: Pattern, v: Value)
      : Unit = pat match{
    case NamePattern(name) => env.update(name, v)
    case TuplePattern(patterns) => v match{
      case TupleValue(vs) if patterns.length == vs.length => 
        for((pat1,v1) <- patterns.zip(vs)) bind(env, pat1, v1)
      case _ => sys.error(s"Bad binding: $pat = $v")
    }
  }

  // ==================================================================

  /** Make an error message. */
  private def mkErr(expected: String, found: Value) = {
    val source = found.source; assert(source != null)
    s"Expected $expected, found value ${found.forError} from \""+
      source.asString+"\""
  }

  /** Write the value of `expr` into cell (c,r), using `d` as the source, but
    * dealing with overwriting of cells.  Handle errors using handleError.  */
  private def writeCell(
    env: Environment, handleError: ErrorValue => Unit, 
    c: Int, r: Int, expr: Exp, d: Directive) 
  = {
    //println(s"writeCell($c $r $expr $d)")
    // val v1 = eval(env, expr).asInstanceOf[Cell].withCellWriteSource(c,r,d) 
    val v1 =
      eval(env, expr).asInstanceOf[Cell].withCSource(CellWriteSource(c,r,d))
    if(env.isEmpty(c,r)){
      //println(s"$v1 ${v1.source}")
      env.setCell(c,r,v1)
      v1 match{ case ev: ErrorValue => handleError(ev); case _ => {} }
    }
    else{
      val mwe = MultipleWriteError(env.getCell1(c,r), v1) 
      env.setCell(c, r, mwe); handleError(mwe)
    }
  }
 
  /** Perform `s` in `env`, handling errors with `handleError`. 
    * @return false if an error occurred in a declaration. */
  private def perform(
    env: Environment, handleError: ErrorValue => Unit, s: Statement)
      : Boolean = s match{
    case d @ Directive(ce, re, expr) => 
      eval(env, ce) match{
        case ColumnValue(c) =>
          if(0 <= c && c < env.width) eval(env, re) match{
            case RowValue(r) =>
              if(0 <= r && r < env.height) 
                writeCell(env, handleError, c, r, expr, d)
              else handleError(EvalError(s"Indexing error for row: found #$r"))
              // end of case RowValue(r)

            case err: ErrorValue => handleError(liftError(s, err))
          } // end of eval(env, re) match
          else handleError(EvalError(
            s"Indexing error for column: found #${CellSource.colName(c)}"))
          // end of case ColumnValue(c)

        case err: ErrorValue => handleError(liftError(s, err))
      } // end of eval(env, ce) match
      // Note: we always return true, even if this particular directive failed
      true

    case ValueDeclaration(pat, exp) => 
      val v = eval(env, exp)
      v match{
        case ev: ErrorValue => handleError(liftError(s, ev)); false
        case _ => bind(env, pat, v); true
      }

    case fd @ FunctionDeclaration(name, tParams, paramss, rt, body) => 
      assert(paramss.nonEmpty)
      env.update(fd.getName, evalFn(env, paramss, body)); true

    case Assertion(condition) => eval(env, condition) match{
      case BoolValue(true) => true
      case BoolValue(false) =>
        val err = liftError(s, EvalError("Assertion error"), true)
        handleError(err); false
      case ev: ErrorValue => handleError(liftError(s, ev)); false
    }

    case Assertion2(condition, msg) => eval(env, condition) match{
      case BoolValue(true) => true
      case BoolValue(false) => eval(env, msg) match{
        case StringValue(st) => 
          val err = liftError(s, EvalError(s"Assertion error: $st"), true)
          handleError(err); false
        case ev: ErrorValue => handleError(liftError(s, ev)); false
      }
      case ev: ErrorValue => handleError(liftError(s, ev)); false
    }

    case ForStatement(binders, stmts) =>
      def he(ev: ErrorValue) = handleError(liftError(s, ev)) 
      performFor(env, he, binders, stmts); true
      // Note: always return true here.

    case CallStatement(e) => eval(env, e) match{
      case UnitValue => true
      case ev: ErrorValue => handleError(liftError(s, ev)); false
    }
  } // end of perform

  /** The Value that represents the function that takes the elements of paramss
    * in turn, and returns body.  This will be a FunctionValue if paramss is
    * non-empty.  */
  private def evalFn(env: Environment, paramss: List[ParameterList], body: Exp)
      : Value = 
    if(paramss.isEmpty) eval(env, body)
    else{
      val params0 = paramss.head
      // Build a Scala function to capture the function of params0.
      def f(args: List[Value]): Value = {
        require(args.length == params0.length)
        // Bind params to values of args in env
        val env2 = env.clone
        for(((x,_),v) <- params0.zip(args)) env2.update(x, v)
        evalFn(env2, paramss.tail, body)
      }
      FunctionValue(f _)
    }

  /** Execute the elements of `statements` in `env`, handling errors with
    * `handleError`.  Stop if an error occurs.  */
  def performAll(
    statements: List[Statement], env: Environment, 
    handleError: ErrorValue => Unit)  = {
    // We evaluate the function declarations first, because it is legal to
    // make a forward reference to a function, and at this point the functions
    // are evaluated lazily (converted into Scala functions).
    val statements1 = 
      statements.filter(_.isInstanceOf[FunctionDeclaration]) ++
        statements.filter(! _.isInstanceOf[FunctionDeclaration])
    var ok = true; val iter = statements1.iterator
    while(ok && iter.hasNext) ok = perform(env, handleError, iter.next())
  }

  /** Execute the for loop "for(binders) stmts". */
  private def performFor(
    env: Environment, handleError: ErrorValue => Unit,
    binders: List[Qualifier], stmts: List[Statement])
      : Unit =
    if(binders.isEmpty) performAll(stmts, env.clone, handleError)
    else binders.head match{
      case Generator(pattern, list) => eval(env, list) match{
        case ListValue(vs) => // bind name to v for each v in vs
          for(v <- vs){
            // Note: need to clone the environment here to prevent leakage.
            val env1 = env.clone; bind(env1, pattern, v) 
            performFor(env1, handleError, binders.tail, stmts)
          }
        case err: ErrorValue => handleError(err)
      }

      case Filter(test) => eval(env, test) match{
        case BoolValue(b) =>
          if(b) performFor(env, handleError, binders.tail, stmts)
        case err: ErrorValue => handleError(err)
      }
    }

  // ========= Testing hooks

  private val outer = this
  object TestHooks{
    val eval = outer.eval _
  }
}
