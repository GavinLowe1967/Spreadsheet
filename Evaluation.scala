package spreadsheet

import scala.collection.mutable.ArrayBuffer

/** The interface of Execution, as seen by an Evaluator object. */
trait ExecutionT{
  /** Execute the elements of `statements` in `env`, handling errors with
    * `handleError`.  Stop if an error occurs.  */
  def performAll(statements: List[Statement], env: Environment, 
    handleError: ErrorValue => Unit): Unit
}

// =======================================================

object Evaluation{
  /** Lift an error value, by tagging on the extent of e. 
    * @param lineNum if true, include line number. */
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

  /** Update env, binding pat to v. */
  def bind(env: Environment, pat: Pattern, v: Value): Unit = pat match{
    case NamePattern(name) => env.update(name, v)
    case TuplePattern(patterns) => v match{
      case TupleValue(vs) if patterns.length == vs.length => 
        for((pat1,v1) <- patterns.zip(vs)) bind(env, pat1, v1)
      case _ => sys.error(s"Bad binding: $pat = $v")
    }
  }
}

// =======================================================

import Evaluation.{liftError,bind}

/** An evaluator for expressions.
  * @param executor An object responsible for executing embedded statements. */
class Evaluation(executor: ExecutionT){
  // ===== Helper functions

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
    }

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
  def eval(env: Environment, e: Exp): Value = {
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
            // Maybe bind name to v
            branch.pattern match{
              case TypedPattern(Some(name), _) => env1.update(name, v)
              case _ => {}
            }
            eval(env1, branch.body)
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
          case Left(vs) => fv(env,vs) match{
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
      executor.performAll(stmts, env1, handleError)
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
}
