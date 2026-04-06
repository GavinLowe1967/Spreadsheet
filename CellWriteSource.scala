package spreadsheet


/** A source corresponding to a write to cell(column, row) by Directive dir. */
case class CellWriteSource(column: Int, row: Int, dir: Directive) extends Source{
  def asString = { 
    val cName = CellSource.colName(column); s"#$cName$row from $dir" 
  }
}


/** A cell written to multiple times. */
case class MultipleWriteError(sources: List[Cell]) extends ErrorValue{
  def msg = 
    "Cell assigned multiple times.\n"+
      sources.map(v => v.source match{
        case CellWriteSource(_,_,d) => 
          val e = d.getExtent
          s"Value ${v.forError} from cell write at line ${e.lineNumber}: "+
            e.asString
        case cs: CellSource => s"User data: ${v.forError}"
        case null => s"null source: $v"
      }
      ).mkString("\n")

  def forError = msg
}

object MultipleWriteError{
  /** Factory method. */
  def apply(c1: Cell, c2: Cell) = c1 match{
    case MultipleWriteError(cells) => new MultipleWriteError(cells:+c2)
    case _ => new MultipleWriteError(List(c1,c2))
  }
}
