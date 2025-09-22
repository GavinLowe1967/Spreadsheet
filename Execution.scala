package spreadsheet

/** Object responsible for evaluating expressions and executing statements. */
object Execution{
  /** Evaluate `e` in environment `env`, adding the extent from `e`. */
  private def eval(env: Environment, e: Exp): Value = e match{
    case  cell @ CellExp(column: Exp, row: Exp, theType: CellType) =>
      /* Read from cell(c,r). */
      def doRead(c: Int, r: Int): Value = {
        val v = env.getCell(c,r); val vType = v.getType
        if(vType == theType) v 
        else{
          val cName = ColumnValue.getName(c)
          e.liftError(TypeError(
            s"Expected $theType, found $vType in cell #$cName$r"))
        }
        // assert(cell.theType != null); val v = env.getCell(c, r)
        // // Note: the call to env.getCell sets the extent. 
        // env.checkType(v, cell.theType) match{
        //   case Ok(()) => v;
        //   case FailureR(msg) =>
        //     val cName = ColumnValue.getName(c)
        //     e.liftError(TypeError(msg+s" in cell (#$cName,#$r)" ))
        // }
      }
      val cc = eval(env, column)
      e.lift({ case ColumnValue(c) =>
        val rr = eval(env, row); e.lift({ case RowValue(r) => doRead(c,r) }, rr)
      }, cc)

    case _ => eval0(env, e).withSource(e.getExtent)
  }

  /** Evaluate `e` in environment `env`. */
  private def eval0(env: Environment, e: Exp): Value = e match{
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
      case fv : FunctionValue =>
        // Evaluate args, but stop if an error occurs
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
        else fv(vs.reverse) match{
          case err: ErrorValue => e.liftError(err); case result => result
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
  } // end of eval

  // ==================================================================

  /** Make an error message. */
  private def mkErr(expected: String, found: Value) = {
    val source = found.source; assert(source != null)
    s"Expected $expected, found value ${found.forError} from \""+
      source.asString+"\""
  }

  /** Perform `s` in `env`, handling errors with `handleError`. 
    * @return false if an error occurred in a declaration. */
  private def perform(
    env: Environment, handleError: ErrorValue => Unit, s: Statement)
      : Boolean = s match{
    case Directive(ce, re, expr) => 
      eval(env, ce) match{
        case ColumnValue(c) =>
          if(0 <= c && c < env.width) eval(env, re) match{
            case RowValue(r) =>
              if(0 <= r && r < env.height){
                if(env.isEmpty(c,r)) eval(env, expr) match{
                  case ev: ErrorValue =>
                    val ev1 = s.liftError(ev); env.setCell(c, r, ev1)
                    handleError(ev1)
                  // Note: ErrorValue <: Cell, so the ordering is important.
                  case v1: Cell => env.setCell(c, r, v1)
                } // end of inner match
                else{
                  val err0 = EvalError("Cell written to for second time")
                  val err = s.liftError(err0, true)
                  env.setCell(c, r, err); handleError(err)
                }
              } // end of outer if
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

    case FunctionDeclaration(name, tParams, params, rt, body) => 
      // Build a Scala function to capture the FunctionDeclaration
      def f(args: List[Value]): Value = {
        require(args.length == params.length)
        // Bind params to values of args in env
        val env2 = env.clone
        for(((x,_),v) <- params.zip(args)) env2.update(x, v)
        eval(env2, body)
      }
      env.update(name, FunctionValue(f _)); true

    case ForStatement(binders, stmts) =>
      def he(ev: ErrorValue) = handleError(s.liftError(ev)) 
      performFor(env, he, binders, stmts); true
      // Note: always return true here.
  } // end of perform


  /** Execute the elements of `statements` in `env`, handling errors with
    * `handleError`.  Stop if an error occurs.
    * @return true if all succeeded.  */
  def performAll(
    statements: List[Statement], env: Environment, 
    handleError: ErrorValue => Unit) 
      : Boolean = {
    var ok = true; val iter = statements.iterator
    while(ok && iter.hasNext) ok = perform(env, handleError, iter.next())
    ok
  }

  /** Execute the for loop "for(binders) stmts". */
  private def performFor(
    env: Environment, handleError: ErrorValue => Unit,
    binders: List[Binder], stmts: List[Statement])
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
