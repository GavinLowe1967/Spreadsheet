package spreadsheet

import FunctionDeclaration.ParameterList
import Evaluation.{liftError,bind}

/** Object responsible for evaluating expressions and executing statements. */
object Execution extends ExecutionT{
  // Functions provided by the Evaluation object
  private val evaluation = new Evaluation(this)
  private val eval = evaluation.eval _
  //private val bind = evaluation.bind _ 

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

    case IfStatement(condition, ifCase, elseCase) => eval(env, condition) match{
      case BoolValue(b) => 
        // Note: clone environment to prevent leakage; and always return true
        performAll(if(b) ifCase else elseCase, env.clone, handleError); true
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

  object TestHooks{
    val eval = evaluation.eval _
  }
}
