package spreadsheet

import scala.collection.mutable.HashMap


/** An environment, for evaluating the spreadsheet.  This principally stores
  * the values of names.
  * @param userCells Array holding the values entered in cells by the user.
  * @param calculatedCells Array recording the values written in cells by the
  * script.
  * @param height The height of the spreadsheet.
  * @param width The width of the spreadsheet.
  * @param nameMap A map from names declared in the script to their values. */
class Environment(
  userCells: Array[Array[Cell]], calculatedCells: Array[Array[Cell]],
  val height: Int, val width: Int, 
  private var nameMap: HashMap[String, Value] 
){
  /* Note: indexing of cells is done by (column, row) coordinates, following the
   * spreadsheet convention. */

  /** Get the value in cell (c,r).  By default, the user's value; otherwise the
    * calculated value. */
  def getCell(c: Int, r: Int): Cell = {
    val v = userCells(c)(r)
    if(v.nonEmpty) v else calculatedCells(c)(r)
  }

  /** Get the user value in cell (c,r).  Called when saving the script. */
  def getUserCell(c: Int, r: Int): Cell = userCells(c)(r)

  /** Set cell (c,r) to store user value cell.  Called by the view, when the
    * user enters a value, or when loading a CSV file. */ 
  def setUserCell(c: Int, r: Int, cell: Cell) = userCells(c)(r) = cell

  /** Is cell (c,r) empty? */
  def isEmpty(c: Int, r: Int): Boolean = 
    userCells(c)(r).isEmpty && calculatedCells(c)(r).isEmpty

  /** Set the value of cell(c,r) to v, and record that it was calculated. */
  def setCell(c: Int, r: Int, v: Cell) = {
    require(isEmpty(c,r) || v.isInstanceOf[MultipleWriteError])
    calculatedCells(c)(r) = v 
  }

  /** Get the value in cell (c,r), prioritising a calculated value. */
  def getCell1(c: Int, r: Int): Cell = {
    val v = calculatedCells(c)(r)
    if(v.nonEmpty) v else userCells(c)(r)
  }

  /** Has a value been calculated for cell (c,r). */
  def isCalculated(c: Int, r: Int) = calculatedCells(c)(r).nonEmpty

  def isError(c: Int, r: Int) = calculatedCells(c)(r).isInstanceOf[ErrorValue]

  /** Get the value to be displayed in the selection box. */
  def getForSelection(c: Int, r: Int): Cell = {
    val uc = userCells(c)(r); val cc = calculatedCells(c)(r)
    if(uc.nonEmpty){
      if(cc.isEmpty) uc else{ assert(cc.isInstanceOf[MultipleWriteError]); cc } 
    }
    else cc
  }

  /** Reset, corresponding to starting to rerun the script. */
  def reset() = {
    for(c <- 0 until width; r <- 0 until height) calculatedCells(c)(r) = Empty() 
    nameMap = Environment.initNameMap
  }

  /** Add name -> v to the environment. */
  def update(name: String, v: Value) = nameMap += (name -> v)

  /** Optionally get the value associated with `name` in the environment. */
  def get(name: String): Option[Value] = nameMap.get(name)

  def apply(name: String): Value = get(name) match{
    case Some(v) => v; case None => sys.error(s"Name not found: $name")
  }

  /** Clone this. */
  override def clone = 
    new Environment(userCells, calculatedCells, height, width, nameMap.clone)

}

// =======================================================

object Environment{

  /** Get the initial nameMap to use in an Environment. */ 
  private def initNameMap = 
    new HashMap[String, Value] ++ BuiltInFunctions.builtIns


  def apply(height: Int, width: Int) = new Environment(
    Array.fill[Cell](width, height)(Empty()),
    Array.fill[Cell](width, height)(Empty()),
    height, width, initNameMap
  )
}
