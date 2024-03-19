package spreadsheet

import scala.collection.mutable.HashMap

/** An environment, mapping identifiers to values. */
class Environment(
  cells: Array[Array[Cell]], calculated: Array[Array[Boolean]],
  val height: Int = 0, val width: Int = 0, 
  private val nameMap: HashMap[String, Value] = Environment.initNameMap
){
  /** Get the value in cell (c,r). */
  def getCell(c: Int, r: Int): Value = cells(c)(r).withSource(CellSource(c,r))

  /** Set the value of cell(c,r) to v, and record that it was calculated. */
  def setCell(c: Int, r: Int, v: Cell) = {
    cells(c)(r) = v; calculated(c)(r) = true
  }

  /** Add name -> v to the environment. */
  def update(name: String, v: Value) = nameMap += (name -> v)

  /** Optionally get the value associated with `name` in the environment. */
  def get(name: String): Option[Value] = nameMap.get(name)

  /** Clone this. */
  override def clone = 
    new Environment(cells, calculated, height, width, nameMap.clone)

}

object Environment{

  /** Get the initial nameMap to use in an Environment. */ 
  private def initNameMap = 
    new HashMap[String, Value] ++ BuiltInFunction.builtIns

}
