package spreadsheet

/** A Statement corresponds to either a definition of a directive in a
  * file. */
trait Statement extends HasExtent{

  /** Perform this in `env`, sending messsages to `view`. */
  def perform(env: Environment, view: ViewT): Boolean 
}

// =======================================================

object Statement{
  def mkErr(expected: String, found: Value) = {
    val source = found.source; assert(source != null)
    s"Expected $expected, found value ${found.forError} from \""+
      source.asString+"\""
  }
}

// =======================================================

/** A simple Directive of the form `Cell(col,row) = exp`. */
case class Directive(cell: CellExp, expr: Exp) extends Statement{
  import Statement.mkErr

  /** Perform this in `env`, sending messsages to `view`. */
  def perform(env: Environment, view: ViewT): Boolean = {
    val CellExp(ce,re) = cell
    ce.eval(env) match{
      case ColumnValue(c) =>
        if(0 <= c && c < env.width) re.eval(env) match{
          case RowValue(r) =>
            if(0 <= r && r < env.height) expr.eval(env) match{
              case ev: ErrorValue =>
                val ev1 = liftError(ev); env.setCell(c, r, ev1)
                println(ev1.msg)
              // Note: ErrorValue <: Cell, so the ordering is important.
              case v1: Cell => env.setCell(c, r, v1) // println(s"($c,$r) = $v1")
              case v => println(v); ??? // FIXME?
            }
            else println("Indexing error for row: found $r")
            // end of case RowValue(r)

          case rr => println(mkErr("row number", rr))
        } // end of re.eval(env) match
          // end of case ColumnValue(c)

        else println("Indexing error for column: found $c")

      case cc => println(mkErr("row identifier", cc))
    } // end of ce.eval(env) match
    true
  }

  override def toString = s"$cell = $expr"
}

// =======================================================

/** A declaration of the form `name = exp`. */
case class ValueDeclaration(name: String, exp: Exp) extends Statement{
  /** Perform this in `env`, sending messsages to `view`. */
  def perform(env: Environment, view: ViewT): Boolean = {
    val v = exp.eval(env)
    v match{
      case ev: ErrorValue => 
        val ev1 = liftError(ev); 
        println(ev1.msg); view.showSelection(ev1.msg); false
      case _ => env.update(name, v); true
    }
  }

}

// =======================================================

/** A declaration of the form { stmt_1; ...; stmt_n; exp }. */ 
case class BlockExp(stmts: List[Statement], exp: Exp) extends Statement{
  /** Perform this in `env`, sending messsages to `view`. */
  def perform(env: Environment, view: ViewT): Boolean = {
    ???

  }
}
