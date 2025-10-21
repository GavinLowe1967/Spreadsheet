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
//  private var typeEnv: EvaluationTypeEnv,
  private var nameMap: HashMap[String, Value] = Environment.initNameMap
){
  /** Get the value in cell (c,r). */
  def getCell(c: Int, r: Int): Cell = {
    val v = userCells(c)(r)
    if(v.getType != EmptyType) v else calculatedCells(c)(r)
  }

  /** Get the user value in cell (c,r).  Called when saving the script. */
  def getUserCell(c: Int, r: Int): Cell = userCells(c)(r)

  /** Set cell (c,r) to store user value cell.  */ 
  def setUserCell(c: Int, r: Int, cell: Cell) = userCells(c)(r) = cell

  //cells(c)(r)//.withCellSource(CellSource(c,r))

  /** Is cell (c,r) empty? */
  def isEmpty(c: Int, r: Int): Boolean = 
    userCells(c)(r).getType == EmptyType && 
      calculatedCells(c)(r).getType == EmptyType

  /** Set the value of cell(c,r) to v, and record that it was calculated. */
  def setCell(c: Int, r: Int, v: Cell) = {
    require(isEmpty(c,r) || v.isInstanceOf[ErrorValue])
    calculatedCells(c)(r) = v // ; calculated(c)(r) = true
  }

  def isCalculated(c: Int, r: Int) = calculatedCells(c)(r).getType != EmptyType //  calculated(c)(r)

  /** Reset, corresponding to starting to rerun the script. */
  def reset() = {
    for(c <- 0 until width; r <- 0 until height) calculatedCells(c)(r) = Empty() 
    nameMap = Environment.initNameMap
  }

  /** Add name -> v to the environment. */
  def update(name: String, v: Value) = nameMap += (name -> v)

  /** Optionally get the value associated with `name` in the environment. */
  def get(name: String): Option[Value] = nameMap.get(name)

  /** Clone this. */
  override def clone = 
    new Environment(userCells, calculatedCells, height, width,  nameMap.clone)

}

// =======================================================

object Environment{

  /** Get the initial nameMap to use in an Environment. */ 
  private def initNameMap = 
    new HashMap[String, Value] ++ BuiltInFunctions.builtIns


  def apply(height: Int, width: Int) = new Environment(
    Array.fill[Cell](width, height)(Empty()),
    Array.fill[Cell](width, height)(Empty()),
    height, width
  )
}
