package spreadsheet

import scala.collection.mutable.HashMap

/** An environment, mapping identifiers to values. */
class Environment(
  cells: Array[Array[Cell]], calculated: Array[Array[Boolean]],
  val height: Int = 0, val width: Int = 0
){
  /** Get the value in cell (c,r). */
  def getCell(c: Int, r: Int): Value = cells(c)(r).withSource(CellSource(c,r))

  def setCell(c: Int, r: Int, v: Cell) = {
    cells(c)(r) = v; calculated(c)(r) = true
  }

  /** Map storing the values of names. */
  private val nameMap = new HashMap[String, Value]

  /** Add name -> v to the environment. */
  def update(name: String, v: Value) = nameMap += (name -> v)

  /** Optionally get the value associated with `name` in the environment. */
  def get(name: String): Option[Value] = nameMap.get(name)

}
