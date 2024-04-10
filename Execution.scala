package spreadsheet

import Statement.mkErr // IMPROVE: move? 

object Execution{
  /** Evaluate `e` in environment `env`, adding the extent from `e`. */
  def eval(env: Environment, e: Exp): Value = e match{
    case  cell @ CellExp(column: Exp, row: Exp) =>
      /* Read from cell(c,r). */
      def doRead(c: Int, r: Int) = {
        assert(cell.theType != null); val v = env.getCell(c, r)
        // Note: the call to env.getCell sets the extent. 
        env.checkType(v, cell.theType) match{
          case Ok(()) => v;
          case FailureR(msg) =>
            val cName = ColumnValue.getName(c)
            e.liftError(TypeError(msg+s" in cell (#$cName,#$r)" ))
        }
      }
      val cc = eval(env, column)
      e.lift({ case ColumnValue(c) =>
        val rr = eval(env, row); e.lift({ case RowValue(r) => doRead(c,r) }, rr)
      }, cc)

    case _ => eval0(env, e).withSource(e.getExtent)
  }

  /** Evaluate `e` in environment `env`. */
  def eval0(env: Environment, e: Exp): Value = e match{
    case NameExp(name) => 
      env.get(name) match{
        case Some(value) => value
        case None => sys.error(s"Name not found: $name")
      }

    case IntExp(value) => IntValue(value)
    case FloatExp(value) => FloatValue(value) 
    case BoolExp(value) => BoolValue(value)
    case StringExp(value) => StringValue(value)
    case RowExp(row) => RowValue(row)
    case c @ ColumnExp(column) => ColumnValue(c.asInt)

    case b @ BinOp(left, op, right) => 
      b.doBinOp(eval(env, left), op, eval(env, right))
      // IMPROVE: move doBinOp

    // case cell @ CellExp(column: Exp, row: Exp) =>
    //   /* Read from cell(c,r). */
    //   def doRead(c: Int, r: Int) = {
    //     assert(cell.theType != null); val v = env.getCell(c, r)
    //     env.checkType(v, cell.theType) match{
    //       case Ok(()) => v;
    //       case FailureR(msg) =>
    //         val cName = ColumnValue.getName(c)
    //         e.liftError(TypeError(msg+s" in cell (#$cName,#$r)" ))
    //     }
    //   }
    //   val cc = eval(env, column)
    //   e.lift({ case ColumnValue(c) =>
    //     val rr = eval(env, row); e.lift({ case RowValue(r) => doRead(c,r) }, rr)
    //   }, cc)

    case IfExp(test, thenClause, elseClause) => 
      eval(env, test) match{
        case BoolValue(true) => eval(env, thenClause)
        case BoolValue(false) => eval(env, elseClause)
        case err: ErrorValue => e.liftError(err)
        case other => sys.error(s"Unexpected type: $other")
      }

    case ListLiteral(elems) => 
      // Traverse elems, evaluating each, and building in vs in reverse;
      // maintain type of elements in theType; and catch any error.
      var es = elems; var vs = List[Value](); var error: ErrorValue = null
      while(es.nonEmpty && error == null){
        eval(env, es.head) match{
          case err: ErrorValue => error = e.liftError(err)
          case v => vs ::= v;  es = es.tail
        }
      }
      if(error != null) error else ListValue(vs.reverse)

    case FunctionApp(f, args) => eval(env, f) match{
      case FunctionValue(params, rt, body, env1) =>
        assert(params.length == args.length) 
        // Try to bind params to values of args in env1; but catch errors
        val env2 = env1.clone; var error: ErrorValue = null
        var iter = params.zip(args).iterator
        while(error == null && iter.hasNext){
          val ((p,_),arg) = iter.next()
          eval(env, arg) match{
            case err: ErrorValue => error = e.liftError(err, true)
            case v =>  env2.update(p, v)
          }
        }
        if(error != null) error
        else eval(env2, body) match{
          case err: ErrorValue => e.liftError(err); case result => result
        }

      case f: BuiltInFunction =>
        var error: ErrorValue = null; val iter = args.iterator
        var vs = List[Value]() // built in reverse
        while(error == null && iter.hasNext){
          val arg = iter.next()
          eval(env, arg) match{
            case err: ErrorValue => error = e.liftError(err, true)
            case v =>  vs ::= v
          }
        }
        if(error != null) error
        else f(vs.reverse) match{
          case err: ErrorValue => e.liftError(err); case result => result
        }
   
      case err: ErrorValue => e.liftError(err)

      case other => sys.error(s"$f -> $other")
    }

    case BlockExp(stmts, exp) => 
      val env1 = env.clone
      // If an error arises in performing stmts, it will be put in err.
      var err: ErrorValue = null
      def handleError(ev: ErrorValue) = { err = ev }
      val ok = performAll(stmts, env1, handleError)
      if(ok){
        assert(err == null)
        eval(env1, exp) match{
          case ev: ErrorValue => e.liftError(ev)
          case res => res
        }
      }
      else e.liftError(err)

    // case _ => e.eval(env)
  }

  // ==================================================================

  /** Perform `s` in `env`, handling errors with `handleError`. 
    * @return false if an error occurred in a declaration. */
  def perform(env: Environment, handleError: ErrorValue => Unit, s: Statement)
      : Boolean = s match{
    case Directive(CellExp(ce,re), expr) => 
      eval(env, ce) match{
        case ColumnValue(c) =>
          if(0 <= c && c < env.width) eval(env, re) match{
            case RowValue(r) =>
              if(0 <= r && r < env.height) eval(env, expr) match{
                case ev: ErrorValue =>
                  val ev1 = s.liftError(ev); env.setCell(c, r, ev1)
                  handleError(ev1)
                // Note: ErrorValue <: Cell, so the ordering is important.
                case v1: Cell => env.setCell(c, r, v1)
                case v => println(v); ??? // IMPROVE?
              }
              else println("Indexing error for row: found $r")
              // end of case RowValue(r)

            case rr => println(mkErr("row number", rr))
          } // end of eval(env, re) match
          else println("Indexing error for column: found $c")

        case cc => println(mkErr("row identifier", cc))
      } // end of eval(env, ce) match
      true

    case ValueDeclaration(name, exp) => 
      val v = eval(env, exp)
      v match{
        case ev: ErrorValue => handleError(s.liftError(ev)); false
        case _ => env.update(name, v); true
      }

    case FunctionDeclaration(name, tParams, params, rt, body) => 
      val fv = FunctionValue(params, rt, body, env)
      env.update(name, fv); true

    // case _ => s.perform(env, handleError)
  }

  /** Execute the elements of `statements` in `env`, handling errors with
    * `handleError`.  Stop if an error occurs.
    * @return true if all succeeded.  */
  def performAll(
    statements: List[Statement], env: Environment, 
    handleError: ErrorValue => Unit) 
      : Boolean = {
    var ok = true; val iter = statements.iterator
    while(ok && iter.hasNext) 
      ok = perform(env.asInstanceOf[Environment], handleError, iter.next())
    ok
  }
}
