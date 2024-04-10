package spreadsheet

/** A Statement corresponds to either a definition of a directive in a
  * file. */
trait Statement extends HasExtent{
  // /** Perform this in `env`, handling errors with `handleError`. 
  //   * @return false if an error occurred in a declaration. */
  // def perform(env: Environment, handleError: ErrorValue => Unit): Boolean 
}

/** Trait of declarations.  These can appear in expression blocks. */
trait Declaration extends Statement

// =======================================================

object Statement{
  def mkErr(expected: String, found: Value) = {
    val source = found.source; assert(source != null)
    s"Expected $expected, found value ${found.forError} from \""+
      source.asString+"\""
  }

  // /** Execute the elements of `statements` in `env`, handling errors with
  //   * `handleError`.  Stop if an error occurs.
  //   * @return true if all succeeded.  */
  // def performAll(
  //   statements: List[Statement], env: EnvironmentT, 
  //   handleError: ErrorValue => Unit) 
  //     : Boolean = {
  //   var ok = true; val iter = statements.iterator
  //   while(ok && iter.hasNext) ok = iter.next().perform(env.asInstanceOf[Environment], handleError)
  //   ok
  // }
}

// import Execution.eval

// =======================================================

/** A simple Directive of the form `Cell(col,row) = exp`. */
case class Directive(cell: CellExp, expr: Exp) extends Statement{
  //import Statement.mkErr

  // /** Perform this in `env`, handling errors with `handleError`. */
  // def perform(env: Environment, handleError: ErrorValue => Unit): Boolean = {
  //   val CellExp(ce,re) = cell
  //   eval(env, ce) match{
  //     case ColumnValue(c) =>
  //       if(0 <= c && c < env.width) eval(env, re) match{
  //         case RowValue(r) =>
  //           if(0 <= r && r < env.height) eval(env, expr) match{
  //             case ev: ErrorValue =>
  //               val ev1 = liftError(ev); env.setCell(c, r, ev1)
  //               handleError(ev1) 
  //             // Note: ErrorValue <: Cell, so the ordering is important.
  //             case v1: Cell => env.setCell(c, r, v1) // println(s"($c,$r) = $v1")
  //             case v => println(v); ??? // IMPROVE?
  //           }
  //           else println("Indexing error for row: found $r")
  //           // end of case RowValue(r)

  //         case rr => println(mkErr("row number", rr))
  //       } // end of re.eval(env) match
  //         // end of case ColumnValue(c)

  //       else println("Indexing error for column: found $c")

  //     case cc => println(mkErr("row identifier", cc))
  //   } // end of ce.eval(env) match
  //   true
  // }

  override def toString = s"$cell = $expr"
}

// =======================================================

/** A declaration of the form `val name = exp`. */
case class ValueDeclaration(name: String, exp: Exp) extends Declaration{
  // /** Perform this in `env`, handling errors with `handleError`.  The effect is
  //   * to update the environment, mapping `name` to the value of `exp`.*/
  // def perform(env: Environment, handleError: ErrorValue => Unit): Boolean = {
  //   val v = eval(env, exp)
  //   v match{
  //     case ev: ErrorValue => 
  //       val ev1 = liftError(ev); 
  //       // println(ev1.msg); // view.showSelection(ev1.msg); 
  //       handleError(ev1); false
  //     case _ => env.update(name, v); true
  //   }
  // }
}

// ========= Note =========
// FunctionValue.scala contains another subclass, FunctionDeclaration.
