package spreadsheet

import FunctionDeclaration.ParameterList
import scala.collection.mutable.ArrayBuffer

/** Object responsible for evaluating expressions and executing statements. */
object Execution{
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

  /** Evaluate `e` in environment `env`, adding the extent from `e`. */
  private def eval(env: Environment, e: Exp): Value = {
    /* If column and row evaluate to a ColumnValue and RowValue, respectively,
     * apply comp to the cell's contents and the cell's name; otherwise give
     * an error. */
    def applyToCell(column: Exp, row: Exp, comp: (Cell,String) => Value)
        : Value = 
      e.lift( { 
        case ColumnValue(c) =>
          e.lift( 
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
        e.liftValue(v1, true)

      case CellMatchExp(column, row, branches) =>
        // Try to match v against branches
        def doMatch(v: Cell, name: String) = {
          val vType = v.getType
          val bs = branches.filter(_.pattern.matches(vType))
          if(bs.isEmpty) 
            e.liftError(TypeError(
              s"Cannot match ${vType.asString} in cell $name"), true)
          else{ 
            val branch = bs.head
            val env1 = env.clone // clone env to prevent leakage
            bind(env1, branch.pattern, v); eval(env1, branch.body)
          }
        }
        applyToCell(column, row, doMatch)

      case uce @ UntypedCellExp(column, row) => 
        // Check contents of cell has type uce.getType
        val v1 = applyToCell(column, row, checkCellType(uce.getType))
        e.liftValue(v1, true)

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
    case RowExp(row) => RowValue(row)
    case c @ ColumnExp(column) => ColumnValue(c.asInt)

    case b @ BinOp(left, op, right) => eval(env, left) match{
      case err1: ErrorValue => e.liftError(err1)
      case v1 => eval(env, right) match{
        case err2: ErrorValue => e.liftError(err2)
        case v2 => BinOpApply(v1, op, v2) match{
          case err: ErrorValue => e.liftError(err, true) // include line number
          case res => res
        }
      }
    }

    case IfExp(test, thenClause, elseClause) => 
      eval(env, test) match{
        case BoolValue(true) => eval(env, thenClause)
        case BoolValue(false) => eval(env, elseClause)
        case err: ErrorValue => e.liftError(err)
        case other => sys.error(s"Unexpected type: $other")
      }

    case ListLiteral(elems) => 
      evalList(env, elems) match{
        case Left(vs) => ListValue(vs); case Right(err) => e.liftError(err)
      }

    case ListComprehension(e1, qs) =>
      val ab = new ArrayBuffer[Value]
      val error = processListComp(env, e1, qs, ab)
      if(error != null) e.liftError(error) else ListValue(ab.toList)

    case TupleLiteral(es) => 
      evalList(env, es) match{
        case Left(vs) => TupleValue(vs); case Right(err) => e.liftError(err)
      }

    case fa @ FunctionApp(f, args) => eval(env, f) match{
      case fv : FunctionValue =>
        evalList(env, args) match{
          case Left(vs) => fv(vs) match{
            case err: ErrorValue => e.liftError(err); case result => result
          }
          case Right(err) => e.liftError(err)
        }
      case err: ErrorValue => e.liftError(err)
      case other => sys.error(s"$f -> $other")
        // Note: eval0 is never called on a CellExpr
      } // end of case FunctionApp(...)

    case BlockExp(stmts, exp) => 
      val env1 = env.clone
      // If an error arises in performing stmts, it will be put in err.
      var err: ErrorValue = null
      def handleError(ev: ErrorValue) = { err = ev }
      val ok = performAll(stmts, env1, handleError)
      if(ok){
        assert(err == null)
        eval(env1, exp) match{
          case ev: ErrorValue => e.liftError(ev); case res => res
        }
      }
      else e.liftError(err)

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
      case Generator(name, list) =>
        eval(env, list) match{
          case ListValue(vs) => 
            // bind name to each element of vs; recurse; and combine results.
            var vs1 = vs; var error: ErrorValue = null
            while(vs1.nonEmpty && error == null){
              val env1 = env.clone; env1.update(name, vs1.head); vs1 = vs1.tail
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
  private def bind(env: Environment, pat: Pattern, v: Cell): Unit = pat match{
    case TypedPattern(Some(name), _) => env.update(name, v)
    case _ => {}
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
    val v1 = eval(env, expr).asInstanceOf[Cell].withCellWriteSource(c,r,d) 
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
              else handleError(EvalError("Indexing error for row: found $r"))
              // end of case RowValue(r)

            case err: ErrorValue => handleError(s.liftError(err))
          } // end of eval(env, re) match
          else handleError(EvalError("Indexing error for column: found $c"))
          // end of case ColumnValue(c)

        case err: ErrorValue => handleError(s.liftError(err))
      } // end of eval(env, ce) match
      // Note: we always return true, even if this particular directive failed
      true

    case ValueDeclaration(name, exp) => 
      val v = eval(env, exp)
      v match{
        case ev: ErrorValue => handleError(s.liftError(ev)); false
        case _ => env.update(name, v); true
      }

    case fd @ FunctionDeclaration(name, tParams, paramss, rt, body) => 
      assert(paramss.nonEmpty)
      env.update(fd.getName, evalFn(env, paramss, body)); true

    case Assertion(condition) => eval(env, condition) match{
      case BoolValue(true) => true
      case BoolValue(false) =>
        val err = s.liftError(EvalError("Assertion error"), true)
        handleError(err); false
      case ev: ErrorValue => handleError(s.liftError(ev)); false
    }

    case Assertion2(condition, msg) => eval(env, condition) match{
      case BoolValue(true) => true
      case BoolValue(false) => eval(env, msg) match{
        case StringValue(st) => 
          val err = s.liftError(EvalError(s"Assertion error: $st"), true)
          handleError(err); false
        case ev: ErrorValue => handleError(s.liftError(ev)); false
      }
      case ev: ErrorValue => handleError(s.liftError(ev)); false
    }

    case ForStatement(binders, stmts) =>
      def he(ev: ErrorValue) = handleError(s.liftError(ev)) 
      performFor(env, he, binders, stmts); true
      // Note: always return true here.
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
    * `handleError`.  Stop if an error occurs.
    * @return true if all succeeded.  */
  def performAll(
    statements: List[Statement], env: Environment, 
    handleError: ErrorValue => Unit) 
      : Boolean = {
    // We evaluate the function declarations first, because it is legal to
    // make a forward reference to a function, and at this point the functions
    // are evaluated lazily (converted into Scala functions).
    val statements1 = 
      statements.filter(_.isInstanceOf[FunctionDeclaration]) ++
        statements.filter(! _.isInstanceOf[FunctionDeclaration])
    var ok = true; val iter = statements1.iterator
    while(ok && iter.hasNext) ok = perform(env, handleError, iter.next())
    ok
  }

  /** Execute the for loop "for(binders) stmts". */
  private def performFor(
    env: Environment, handleError: ErrorValue => Unit,
    binders: List[Qualifier], stmts: List[Statement])
      : Unit =
    if(binders.isEmpty) performAll(stmts, env.clone, handleError)
    else binders.head match{
      case Generator(name, list) => eval(env, list) match{
        case ListValue(vs) => // bind name to v for each v in vs
          for(v <- vs){
            // Note: need to clone the environment here to prevent leakage.
            val env1 = env.clone; env1.update(name, v)
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
