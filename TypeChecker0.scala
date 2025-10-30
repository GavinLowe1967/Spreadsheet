package spreadsheet

/* This file contains some basics of the type checker. */

import Unification.unify

object TypeChecker0{
  /** The result of typechecking an expression. */
  type TypeCheckRes = Reply[(TypeEnv,TypeT)]

  /** Check that all UntypedCellExps have been given a concrete type, and remove
    * those cells from the type environment.. */
  def close(typeEnv: TypeEnv, t: TypeT): TypeCheckRes = {
    val untypedCells = typeEnv.getUntypedCells
    if(untypedCells.isEmpty) Ok(typeEnv.removeUntypedCells, t)
    else{
      val s = if(untypedCells.length > 1) "s" else ""
      FailureR(
        s"Couldn't find type$s for cell expression$s "+
          untypedCells.map(_.getExtent.asString).mkString(", ")
      )
    }
  }
}

import TypeChecker0._

// =======================================================

/** Interface of ExpTypeChecker, as seen by BinOpTypeChecker. */
trait ExpTypeCheckerT{
  /** Typecheck exp. */
  def typeCheck(typeEnv: TypeEnv, exp: Exp): TypeCheckRes

  /** Typecheck exp, and unify with eType. */
  def typeCheckUnify(typeEnv: TypeEnv, exp: Exp, eType: TypeT)
      : TypeCheckRes
}

// =======================================================

/** Type checker for binary operations.  
  * @param etc object to use to recursively typecheck subexpressions.  */
class BinOpTypeChecker(etc: ExpTypeCheckerT){
  private val typeCheckUnify = etc.typeCheckUnify _

  /** Make a String representing a disjunction: "<X>, <Y>, ... or <Z>". */
  private def mkDisjunction(ts: List[TypeT]): String = ts match{
    case List() => sys.error("Empty list in mkDisjunction") // Can't happen
    case List(t) => t.asString
    case List(t1, t2) => t1.asString+" or "+t2.asString
    case t::ts1 => t.asString+", "+mkDisjunction(ts1)
  }

  /** Map giving all types for infix operators, except for equality,
    * inequality and (::). */
  private val binopTypes: Map[String, List[(TypeT,TypeT,TypeT)]] = {
    val numeric = // numeric operators
      List((IntType,IntType,IntType), (FloatType,FloatType,FloatType))
    val arith =  // + and -
      numeric ++ List((RowType,IntType,RowType), (ColumnType,IntType,ColumnType))
    val order = // order relations; TODO: add Row, Column
      List((IntType,IntType,BoolType), (FloatType,FloatType,BoolType))
    val bool = List((BoolType,BoolType,BoolType))
    val enumT = // enumerable types
      for(t <- List(IntType,RowType,ColumnType)) yield (t,t,ListType(t))
    Map(
      "+" -> arith, "-" -> arith, "*" -> numeric, "/" -> numeric,
      "<" -> order, "<=" -> order, ">" -> order, ">=" -> order,
      "&&" -> bool, "||" -> bool, "to" -> enumT, "until" -> enumT
    )
  }

  /** Typecheck BinOp(left, op, right). */
  def typeCheckBinOp(typeEnv: TypeEnv, left: Exp, op: String, right: Exp)
      : TypeCheckRes =
    etc.typeCheck(typeEnv, left).map{ case (te1, tl) =>
      op match{
        case "==" | "!=" =>
          // Check tl is a concrete equality type
          close(te1,tl).map{ case (te2,`tl`) =>
            te2.mkEqType(tl).map{ te3 => 
              typeCheckUnify(te3, right, tl).map{ case (te4, tr) =>
                // Note, in the case of "[] == [f]" for f not an equality type, 
                // unification fails.
                Ok((te4,BoolType))
              }.lift(right,true)
            }
          }
        case "::" =>
          typeCheckUnify(te1, right, ListType(tl))
        case _ => // Overloaded operator
          val ts = binopTypes(op)
          if(ts.length == 1){ // Unify tl with expected type
            val (etl,etr,rt) = ts.head
            unify(te1, tl, etl).map{ case (te2, `etl`) =>
              typeCheckUnify(te2, right, etr).map{ case (te3, `etr`) =>
                Ok((te3, rt)) }
            }
          }
          else // tl should match a first field of a member of ts
            close(te1,tl).map{ case (te2,`tl`) => ts.filter(_._1 == tl) match{
              case List((`tl`,etr,rt)) =>
                typeCheckUnify(te2, right, etr).map{ case (te3, `etr`) =>
                  Ok((te3, rt)) }
              case List() =>
                FailureR("Expected "+mkDisjunction(ts.map(_._1))+
                  ", found "+tl.asString).lift(left)
              case _ => sys.error(s"($left,$op,$right)")// Can't happen
            } }
      } // end of "op match"
    }
}

// =======================================================

/** Type checker for cell reads.  
  * @param etc object to use to recursively typecheck subexpressions.  */
class CellReadTypeChecker(etc: ExpTypeCheckerT){
  val typeCheckUnify = etc.typeCheckUnify _
  val typeCheck = etc.typeCheck _

  /** Check `column` and `row` produce a ColumnType and RowType respectively; if
    * so, apply `f` to the resulting TypeEnv. */ 
  def checkCellRead(
    typeEnv: TypeEnv, column: Exp, row: Exp, f: TypeEnv => TypeCheckRes)
      : TypeCheckRes = 
    typeCheckUnify(typeEnv, column, ColumnType).map{ case (te1, ColumnType) =>
      typeCheckUnify(te1, row, RowType).map{ case (te2, RowType) => f(te2) }
    }

// TODO: check for repeated patterns
  /** Typecheck the branches of a cell match expression. */
  def typeCheckBranches(typeEnv: TypeEnv, branches: List[MatchBranch])
      : TypeCheckRes = {
    assert(branches.nonEmpty) // caught by parser
    val b1 = branches.head
    typeCheckBranch(typeEnv, b1).mapOrLift(b1, { case(te1,t1) =>
      typeCheckUnifyBranches(te1, branches.tail, t1)
    })
  }

  /** Typecheck a single branch of a cell match expression. */
  private def typeCheckBranch(typeEnv: TypeEnv, branch: MatchBranch)
      : TypeCheckRes = {
    val MatchBranch(pat, body) = branch
    pat match{
      case TypedPattern(Some(name), t) => 
        // Check body in new scope, with name -> t
        typeCheck(typeEnv.newScope+(name,t), body).map{ case (te1, t1) =>
          Ok((te1.endScope, t1))
        }
      case _ => typeCheck(typeEnv, body)
    }
  }

  /** Typecheck branches, and unify with t. */
  private def typeCheckUnifyBranches(
    typeEnv: TypeEnv, branches: List[MatchBranch], t: TypeT)
      : TypeCheckRes =
    if(branches.isEmpty) Ok((typeEnv, t))
    else{
      val b1 = branches.head
      typeCheckBranch(typeEnv, b1).map{ case (te1,t1) =>
        unify(te1, t1, t).mapOrLift(b1.body, { case (te2, t2) =>
          typeCheckUnifyBranches(te2, branches.tail, t2)
        }).lift(b1)
      }
    }
}
