package spreadsheet

/** The model. */
class Model(val height: Int, val width: Int){
  /** The View, as seen from the Model. */ 
  private var view: ViewT = null

  /** Set the view to be `v`.  */
  def setView(v: ViewT) = view = v

  /** The cells holding the content.  Note: indexing is done by (row,
    * coordinate), following the spreadsheet convention. */
  val cells = Array.fill[Cell](width, height)(Empty())

  /** Record of which cells were calculated. */
  val calculated = Array.fill(width, height)(false)

  /** The statements defined in a file.  Set by loadFile. */
  private var statements = List[Statement]()

  /** Load statements from `fname`. */
  def loadFile(fname: String) = {
    val fContents = scala.io.Source.fromFile(fname).mkString
    // println(fContents)
    StatementParser.parseStatements(fContents) match{
      case Left(ss) => statements = ss
      case Right(msg) => println(s"Error!$msg")
    }
    update()
  }

  /** Update all cells based on statements. */
  def update() = {
    val env = new Environment(cells)
    for(stmt <- statements) stmt match{
      case dir: Directive =>  performDirective(env, dir)
      case valDec: ValueDeclaration => processValDec(env, valDec)
    }
  }

  /** Perform directive `dir` within `env`. */
  private def performDirective(env: Environment, dir: Directive) = {
    val Directive(CellExp(ce,re), expr) = dir
    ce.eval(env) match{
      case ColumnValue(c) =>
        if(0 <= c && c < width) re.eval(env) match{
          case RowValue(r) =>
            if(0 <= r && r < height) expr.eval(env) match{
              case ev: ErrorValue =>
                cells(c)(r) = ev; calculated(c)(r) = true; println(ev.msg)
              // Note: ErrorValue <: Cell, so the ordering is important.
              case v1: Cell =>
                println(s"($c,$r) = $v1")
                cells(c)(r) = v1; calculated(c)(r) = true
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
  }

  private def processValDec(env: Environment, valDec: ValueDeclaration) = {
    println(valDec)
    val ValueDeclaration(name, e) = valDec
    val v = e.eval(env); println(v)
    env.update(name, v)
  }

  private def mkErr(expected: String, found: Value) = {
    val source = found.source; assert(source != null)
    s"Expected $expected, found value ${found.forError} from \""+
      source.asString+"\""
  }


}
