package spreadsheet

/* This file contains some basics of the type checker, and type checkers
 * responsible for type checking particular syntactic forms.. */

import Unification.unify
import TypeVar.TypeID // Type variables (Ints)

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

  /** The next type identifier to use. */
  private var next = 0

  /** Get a new type identifier. */
  def nextTypeID() : TypeID = TypeVar.getNext()
}

import TypeChecker0._

// =======================================================

/** Interface of ExpTypeChecker, as seen by BinOpTypeChecker,
  * CellReadTypeChecker and FunctionAppTypeChecker. */
trait ExpTypeCheckerT{
  /** Typecheck exp. */
  def typeCheck(typeEnv: TypeEnv, exp: Exp): TypeCheckRes

  /** Typecheck exp, and unify with eType. */
  def typeCheckUnify(typeEnv: TypeEnv, exp: Exp, eType: TypeT): TypeCheckRes
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

// =======================================================

/** Type checking of function applications. */
class FunctionAppTypeChecker(etc: ExpTypeCheckerT){
  import FunctionType.TypeParameter // (TypeParamName, TypeParamConstraint)
  import TypeParam.TypeParamName // Names of type parameters (Strings)
  import NameExp.Name // Names of identifiers (Strings)

  private var nextNameIx = 0

  /** Get a new Name. */
  private def newName(): Name = { nextNameIx += 1; "%"+nextNameIx } 

  /** Typecheck the application of a value of type t (not necessarily a
    * FunctionType) to args. */
  def checkFunctionApp(typeEnv: TypeEnv, t: TypeT, args: List[Exp])
      : TypeCheckRes =
    t match{
      case ft: FunctionType => checkFunctionApp1(typeEnv, ft, args)
      case _ => FailureR("Non-function applied as function")
    }

  /** Typecheck the application of a function of type ft to args. */
  private 
  def checkFunctionApp1(typeEnv: TypeEnv, ft: FunctionType, args: List[Exp])
      : TypeCheckRes = {
    val FunctionType(tParams, domain, range) = ft
    if(domain.length != args.length)
      FailureR(s"Expected ${domain.length} arguments, found "+args.length)
    else{
      // Create fresh type variables to replace tParams in domain and range
      val (te1, domain1, range1) =
        subTypeParams(typeEnv.newScope, tParams, domain, range)
      // Generate a new name, and bind it to range1 in the environment;
      // then unify the types of args with domain1, so the new name gets
      // updated to the appropriate return type.
      val name = newName(); val te2 = te1 + (name, range1)
      typeCheckListUnify(te2, args, domain1).map{ te3 =>
        Ok((te3.endScope, te3(name)))  // extract type of name
      }
    }
  }

  /** Replace each type parameter in tParams with a fresh type variable,
    * substituting in domain and range, and adding suitable constraints to
    * typeEnv). */
  private def subTypeParams(typeEnv: TypeEnv, 
    tParams: List[TypeParameter], domain: List[TypeT], range: TypeT)
      : (TypeEnv, List[TypeT], TypeT) = {
    // Create fresh type variables to replace tParams in domain and range
    var updates = List[(TypeParamName, TypeVar)]();
    var constraints = List[(TypeID, TypeConstraint)]()
    for((p,c) <- tParams){
      val tId = nextTypeID(); updates ::= (p, TypeVar(tId))
      constraints ::= (tId,c)
    }
    // The instantiated domain and range
    val (domain1,range1) = Substitution.remapBy(updates, domain, range)
    (typeEnv.addConstraints(constraints), domain1, range1)
  }

  /** Type check each element of es, unifying its type with the corresponding
    * element of ts. 
    * Pre: es.length == ts.length.
    * Used to check actual parameters es of a function application against the
    * expected types ts.  */
  private 
  def typeCheckListUnify(typeEnv: TypeEnv, es: List[Exp], ts: List[TypeT])
      : Reply[TypeEnv] = 
    if(es.isEmpty){ assert(ts.isEmpty); Ok(typeEnv) }
    else etc.typeCheck(typeEnv, es.head).map{ case (te1,t1) => 
      // If t1 is a FunctionType, instantiate type parameters
      val (te2,t2) = mkInstance(te1,t1) 
      // Unify with formal parameter type in ts, and recurse.
      unify(te2, t2, ts.head).lift(es.head, true).map{ case (te3, _) => 
        typeCheckListUnify(te3, es.tail, ts.tail)
      }
    }

  /** If t is a FunctionType, make an instance of it, replacing each type
    * parameter by a fresh type variable, and add suitable constraints to
    * typeEnv. */
  private def mkInstance(typeEnv: TypeEnv, t: TypeT): (TypeEnv, TypeT) = t match{
    case FunctionType(tParams, domain, range) => 
      val (te, domain1, range1) = subTypeParams(typeEnv, tParams, domain, range)
      (te, FunctionType(List(), domain1, range1))
    case _ => (typeEnv, t)
  }

  /** Type check es in turn.  If successful, return the resulting type
    * environment and list of types. */
  private def typeCheckList(typeEnv: TypeEnv, es: List[Exp])
      : Reply[(TypeEnv, List[TypeT])] = {
    if(es.isEmpty) Ok(typeEnv, List[TypeT]())
    else etc.typeCheck(typeEnv, es.head).map{ case(te1, t1) => 
      val (te2,t2) = mkInstance(te1,t1)
      typeCheckList(te2, es.tail).map{ case (te3,ts) => Ok(te3, t2::ts) }
    }
  }


  /** Typecheck args, and find the FunctionType in ts that can be applied to
    * args, if any.  If successful, return the index, resulting environment,
    * and the type of the function application. */
  def findFunctionApp(typeEnv: TypeEnv, ts: Array[FunctionType], args: List[Exp])
      : Reply[(Int, TypeEnv, TypeT)] = {
    assert(ts.forall(_.params.isEmpty))
    // Get types of actual parameters
    typeCheckList(typeEnv.newScope, args).map{ case (te1, argsTs) => 
      // Find those elements that match
      (0 until ts.length).toList.filter(i => ts(i).domain == argsTs) match{
        case List() => 
          FailureR("Overloaded function application with types\n"+
            ts.map(_.asString).mkString(", ")+"\ncan't be applied to argument"+
            (if(args.length > 1) "s" else "")+" of type "+
            argsTs.map(_.asString).mkString(", "))
        case List(index) =>  Ok((index, te1.endScope, ts(index).range))
        case _ => ??? // I think this can't happen
// FIXME
      }
    }
  }
  // private def tryMatch(
  //   typeEnv: TypeEnv, domain: List[TypeT], range: TypeT, args: List[Exp])
  //     : TypeCheckRes = {
  //   val name = newName(); val te2 = typeEnv + (name, range1)
  //   typeCheckListUnify(te2, args, domain1).map{ te3 =>
  //     Ok((te3.endScope, te3(name)))  // extract type of name
  //   }
  // }


}
