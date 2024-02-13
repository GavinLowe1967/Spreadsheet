package spreadsheet

/** A Statement corresponds to either a definition of a directive in a
  * file. */
trait Statement

/** A simple Directive of the form `Cell(col,row) = exp`. */
case class Directive(cell: CellExp, exp: Exp) extends Statement{
  override def toString = s"$cell = $exp"

}

case class ValueDeclaration(name: String, exp: Exp) extends Statement
