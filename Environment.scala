package spreadsheet

/** An environment, mapping identifiers to values. */
class Environment(cells: Array[Array[Cell]]){
  // TODO: also need definitions from the script


  def getCell(c: Int, r: Int): Value = cells(c)(r)
}
