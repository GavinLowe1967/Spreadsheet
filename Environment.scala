package spreadsheet

import scala.collection.mutable.HashMap


/** An environment, for evaluating the spreadsheet.  This principally stores
  * the values of names.
  * @param cells Array holding the values of cells.
  * @param calculated Array recording which cells were calculated via the 
  * script.
  * @param height The height of the spreadsheet.
  * @param width The width of the spreadsheet.
  * @param typeEnv the type environment to use when checking types in cells. 
  * @param typeChecker the type checker to use. */
class Environment(
  cells: Array[Array[Cell]], calculated: Array[Array[Boolean]],
  val height: Int, val width: Int, 
  private var typeEnv: TypeEnv,
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

  /** Check that v has type t. 
    * Called from CellExp.eval. */
  def checkType(v: Value, t: TypeT): Reply[Unit] = {
    EvaluationTypeChecker.unify(typeEnv, v.getType, t).map{ case(te,tt) =>
      // Note: the new type environment is stored, for use in subsequent steps
      // of the current evaluation.
      // println(s"$v -> $t")
      typeEnv = te; Ok(()) 
    }
  }

  /** Clone this. */
  override def clone = 
    new Environment(cells, calculated, height, width, typeEnv, nameMap.clone)
}

// =======================================================

object Environment{

  /** Get the initial nameMap to use in an Environment. */ 
  private def initNameMap = 
    new HashMap[String, Value] ++ BuiltInFunction.builtIns

}
