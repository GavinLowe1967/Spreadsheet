package spreadsheet

import scala.collection.mutable.HashMap

/** A class representing results from typechecking. */
abstract class Reply[+A]{
  /** If successful, apply `f` to the contents of this. */
  def map[B](f: A => Reply[B]): Reply[B]

  /** If successful, apply `f` to the contents of this.  If a failure, add the
    * source of `exp` to the message. */
  def mapOrLift[B](exp: HasExtent, f: A => Reply[B]): Reply[B]

  /** If a failure, add the source of `exp` to the message. */
  def lift(exp: HasExtent, lineNum: Boolean = false): Reply[A]
}

/** The result of a successful typechecking, corresponding to x. */
case class Ok[+A](x: A) extends Reply[A]{
  def map[B](f: A => Reply[B]) = f(x)

  def mapOrLift[B](exp: HasExtent, f: A => Reply[B]) = f(x)

  def lift(exp: HasExtent, lineNum: Boolean = false) = this
}

/** The result of an unsuccessful typechecking, as explained by err. */
case class FailureR(err: String) extends Reply[Nothing]{
  def map[B](f: Nothing => Reply[B]) = this // FailureR(err)

  def mapOrLift[B](exp: HasExtent, f: Nothing => Reply[B]) = lift(exp)
  //  FailureR(err+"\n\tin "+exp.getExtent.asString)

  /** Add the source of `exp` to the message. */
  def lift(exp: HasExtent, lineNum: Boolean = false) = {
    val extent = exp.getExtent
    val lnString = if(lineNum) " at line "+extent.lineNumber else ""
    FailureR(err+lnString+"\n\tin "+extent.asString)
  }
}

// ==================================================================

import TypeVar.TypeID
import Substitution.TypeMap

/** Abstractly, a mapping from type variables to type expressions (TypeT), as
  * captured by map. 
  * @param map The mapping from identifiers of type variables to the 
  * corresponding type expression.  Any identifier not in the map implicitly
  * maps to itself.*/
class Substitution(private var map: TypeMap){

  /** The value associated with tv.  Testing only. */
  private def apply(tv: TypeID) = map(tv)

  /** Clear the map.  For testing only. */
  private def clear() = map.clear()

  /** Apply this substitution to t. */
  def applyTo(t: TypeT): TypeT = t match{
    case TypeVar(tv) => 
      map.get(tv) match{ case Some(t1) => t1; case None => t }
    case ListType(underlying) => ListType(applyTo(underlying))
    case FunctionType(domain, range) => 
      FunctionType(domain.map(applyTo), applyTo(range))
    case _ => t
  }

  /** Extend this according to the constaint TypeVar(tv) = t, unless that
    * creates an infinite type. */
  private def extend(tv: TypeID, t: TypeT): Reply[Unit] = {
    require(!map.contains(tv))
    require(applyTo(t) == t)
    if(t == TypeVar(tv)) Ok(())
    else if(t.typeVars.contains(tv)) FailureR(s"Can't unify $t with $tv")
    else{ 
      // For each (tv1 -> t1) in map, apply the substitution tv -> t to t1.
      map = map.map{ case (tv1,t1) => (tv1, Substitution.reMap(tv, t, t1)) } 
      map += tv -> t; Ok(())
    } 
  }

  /** Extend this to unify t1 and t2. */
  def unify(t1: TypeT, t2: TypeT): Reply[Unit] = (t1,t2) match{
    case (TypeVar(tv), _) => 
      map.get(tv) match{
        case None => extend(tv, applyTo(t2))
        case Some(t11) => unify(t11, applyTo(t2))
      }

    case (_, TypeVar(_)) => unify(t2,t1) 
    
    case (IntType, IntType) | (BoolType, BoolType) | (StringType, StringType) =>
      // FIXME: other cases
      Ok(())

    case (ListType(u1), ListType(u2)) => unify(u1, u2)

    case (FunctionType(d1,r1), FunctionType(d2,r2)) => 
      val len = d1.length
      if(len != d2.length) FailureR(s"Cannot unify $t1 with $t2")
      else{
        var res = unify(r1,r2); var i = 0
        while(i < len && res == Ok(())){
          res = unify(d1(i), d2(i)); i += 1
        }
        res
      }

    case _ => FailureR(s"Cannot unify $t1 with $t2")
  }

}

object Substitution{

  /** Apply the substitution tv -> t to t1. */ 
  def reMap(tv: TypeID, t: TypeT, t1: TypeT): TypeT = t1 match{
    case TypeVar(tv1) => if(tv1 == tv) t else t1
    case ListType(underlying) => ListType(reMap(tv, t, underlying))
    case FunctionType(domain, range) =>
      FunctionType(domain.map(reMap(tv, t, _)), reMap(tv, t, range))
    case _ => t1
  }


  type TypeMap = HashMap[TypeID, TypeT]

  /** Testing. */
  def main(args: Array[String]) = {
    val sub = new Substitution(new TypeMap)
    def ok(r: Reply[Unit]) = r == Ok(())
    assert(ok(sub.unify(FunctionType(List(IntType), BoolType), TypeVar(0))))
    assert(!ok(sub.unify(TypeVar(0), IntType)))
    assert(ok(sub.unify(TypeVar(0), FunctionType(List(TypeVar(1)), TypeVar(2)))))
    assert(sub(1) == IntType); assert(sub(2) == BoolType)
    assert(!ok(sub.unify(FunctionType(List(TypeVar(3)), BoolType), TypeVar(3))))
    assert(!ok(sub.unify(FunctionType(List(IntType), TypeVar(3)), TypeVar(3))))

    sub.clear()
    assert(ok(sub.unify(TypeVar(0), FunctionType(List(TypeVar(1)), TypeVar(2)))))
    assert(ok(sub.unify(TypeVar(1), IntType)))
    assert(ok(sub.unify(TypeVar(3), TypeVar(2))))
    assert(ok(sub.unify(TypeVar(2), TypeVar(3))))
    assert(ok(sub.unify(TypeVar(3), BoolType)))
    assert(sub(0) == FunctionType(List(IntType), BoolType))

    println("Done")
  }

}
