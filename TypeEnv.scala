package spreadsheet

import scala.collection.immutable.{Map,HashMap}

  import TypeVar.TypeID // Type variables (Ints)
  import NameExp.Name // Names of identifiers (Strings)

object TypeEnv{
  /** A mapping from names in the script to their types. */
  type NameMap = HashMap[Name, TypeT]

  /** A mapping from type identifiers to MemberOf(ts) constraints. */
  type Constraints = HashMap[TypeID, TypeConstraint]
}
// IMPROVE: do we need the MemberOf constructors?  

import TypeEnv._

/** A type environment.
  * @param nameMap A mapping from names in the script to their types.
  * @param constraints A mapping from type identifiers to MemberOf(ts) constraints.
  * Inv: constraints includes a mapping tid -> MemberOf(ts) for each
  * TypeVar(tid) used in nameMap. 
  * Note: type environments are treated immutably. */
case class TypeEnv(nameMap: NameMap, constraints: Constraints){
  /** The type associated with name. */
  def apply(name: Name) = nameMap(name)

  /** The constraint associated with tid. */
  def apply(tid: TypeID) = constraints(tid)

  /** The TypeEnv formed by adding the mapping name -> t. */
  def + (name: Name, t: TypeT) = TypeEnv(nameMap + (name -> t), constraints)

  /** The TypeEnv formed by adding each name -> t for (name,t) in pairs. */
  def ++ (pairs: List[(Name, TypeT)]) = TypeEnv(nameMap ++ pairs, constraints)

  /** The TypeEnv formed by adding  the constraint typeID -> tc. */
  def + (typeID: TypeID, tc: TypeConstraint) = {
    tc match{ case MemberOf(ts) => require(ts.length >= 2, tc) }
    TypeEnv(nameMap, constraints + (typeID -> tc))
  }

  /** The TypeEnv formed from this by replacing TypeVar(tId) with t. */
  def replace(tID: TypeID, t: TypeT): TypeEnv = {
    val newNameMap =
      nameMap.map{ case (n,t1) => (n, Substitution.reMap(tID, t, t1)) }
    TypeEnv(newNameMap, constraints - tID)
  }
}
