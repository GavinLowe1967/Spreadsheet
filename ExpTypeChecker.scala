package spreadsheet

/** The interface of DeclarationTypeChecker, as seen by ExpTypeChecker. */
trait DeclarationTypeCheckerT{
  /** Type check decls, returning the resulting type environment if
    * successful. */
  def typeCheckDeclList(typeEnv: TypeEnv, decls: List[Declaration])
      : Reply[TypeEnv] 
}

// =======================================================

import TypeVar.TypeID // Type variables (Ints)
import TypeParam.TypeParamName // Names of type parameters (Strings)

import NameExp.Name // Names of identifiers (Strings)

object ExpTypeChecker{
  /** Contents of the result of a successful call to typeCheck. */
  type TypeCheckRes = (TypeEnv,TypeT)

  /** The next type identifier to use. */
  private var next = 0

  /** Get a new type identifier. */
  private def nextTypeID() : TypeID = TypeVar.getNext()

  private var nextNameIx = 0

  /** Get a new Name. */
  private def newName(): Name = { nextNameIx += 1; "%"+nextNameIx } 

  /** Check that all UntypedCellExps have been given a concrete type. */
  def close(typeEnv: TypeEnv, t: TypeT): Reply[TypeCheckRes] = {
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

// =======================================================

trait ExpTypeCheckerT{
  import ExpTypeChecker.TypeCheckRes
  def typeCheck(typeEnv: TypeEnv, exp: Exp): Reply[TypeCheckRes]
  def typeCheckUnify(typeEnv: TypeEnv, exp: Exp, eType: TypeT)
      : Reply[TypeCheckRes]
}

// Note: a single ExpTypeChecker object is created, in DeclarationTypeChecker. 

/** Type checker for expressions. */
class ExpTypeChecker(dtc: DeclarationTypeCheckerT) extends ExpTypeCheckerT{
  import FunctionType.TypeParameter // (TypeParamName, TypeParamConstraint)
  import Unification.unify
  import ExpTypeChecker.{TypeCheckRes,nextTypeID,newName,close}

  //import DeclarationTypeChecker.typeCheckDeclList

  private val botc = new BinOpTypeChecker(this)



  // Cell reads

  /** Check `column` and `row` produce a ColumnType and RowType respectively; if
    * so, apply `f` to the resulting TypeEnv. */ 
  private def checkCellRead(
    typeEnv: TypeEnv, column: Exp, row: Exp, f: TypeEnv => Reply[TypeCheckRes])
      : Reply[TypeCheckRes] = 
    typeCheckUnify(typeEnv, column, ColumnType).map{ case (te1, ColumnType) =>
      typeCheckUnify(te1, row, RowType).map{ case (te2, RowType) => f(te2) }
    }

  // ===== Cell match expressions

// TODO: check for repeated patterns
  /** Typecheck the branches of a cell match expression. */
  private def typeCheckBranches(typeEnv: TypeEnv, branches: List[MatchBranch])
      : Reply[TypeCheckRes] = {
    assert(branches.nonEmpty) // caught by parser
    val b1 = branches.head
    typeCheckBranch(typeEnv, b1).mapOrLift(b1, { case(te1,t1) =>
      typeCheckUnifyBranches(te1, branches.tail, t1)
    })
  }

  /** Typecheck a single branch of a cell match expression. */
  private def typeCheckBranch(typeEnv: TypeEnv, branch: MatchBranch)
      : Reply[TypeCheckRes] = {
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
      : Reply[TypeCheckRes] =
    if(branches.isEmpty) Ok((typeEnv, t))
    else{
      val b1 = branches.head
      typeCheckBranch(typeEnv, b1).map{ case (te1,t1) =>
        unify(te1, t1, t).mapOrLift(b1.body, { case (te2, t2) =>
          typeCheckUnifyBranches(te2, branches.tail, t2)
        }).lift(b1)
      }
    }

  // ===== Type checking of expression

  /** Typecheck expression `exp` in type environment `typeEnv`.
    * @return a Reply, if successful, the updated type environment and the 
    * type of exp. */
  def typeCheck(typeEnv: TypeEnv, exp: Exp)
      : Reply[TypeCheckRes] = exp match{
    case NameExp(n) => typeEnv.get(n) match{
      case Some(t) => Ok((typeEnv,t))
      case None => FailureR("Name not found").lift(exp, true)
    }
    // Atomic types
    case IntExp(v) => Ok((typeEnv,IntType))
    case FloatExp(v) => Ok((typeEnv,FloatType))
    case BoolExp(v) => Ok((typeEnv,BoolType))
    case StringExp(st) => Ok((typeEnv,StringType))
    case RowExp(row) => Ok((typeEnv, RowType))
    case ColumnExp(column) => Ok((typeEnv, ColumnType))
    // Binary operators  
    case BinOp(left, op, right) => 
      botc.typeCheckBinOp(typeEnv, left, op, right).lift(exp)
    // Typed cell expressions
    case ce @ CellExp(column, row, theType) =>
      checkCellRead(typeEnv, column, row, te => Ok(te, theType)).lift(exp)
    // Untyped cell expressions
    case cell @ UntypedCellExp(column, row) =>   
      def setType(te: TypeEnv) = {
        val ct = CellTypeVar(nextTypeID()); cell.setTypeVar(ct); Ok(te+cell, ct)
      }
      checkCellRead(typeEnv, column, row, setType).lift(exp)
    // Cell match expressions
    case CellMatchExp(column, row, branches) => 
      checkCellRead(
        typeEnv, column, row, te => typeCheckBranches(te, branches)
      ).lift(exp)
    // Conditionals
    case IfExp(test, thenClause, elseClause) =>
      typeCheckUnify(typeEnv, test, BoolType).map{ case (te1, bt) =>
        assert(bt == BoolType)
        typeCheckAndClose(te1, thenClause).map{ case (te2,t1) =>
          typeCheckUnify(te2, elseClause, t1)
        }
      }.lift(exp)
    // List literals
    case ListLiteral(elems) => 
      if(elems.isEmpty){
        // Associate type identifier with this list.
        val typeId = nextTypeID()
        Ok((typeEnv + (typeId, AnyTypeConstraint), ListType(TypeVar(typeId))))
      }
      else typeCheck(typeEnv, elems.head).map{ case (te1, t1) =>
        // Try to unify types of remainder with t1
        typeCheckListSingleType(te1, elems.tail, t1).lift(exp)
      }
    // Function applications
    case FunctionApp(f, args) => 
      typeCheck(typeEnv, f).map{ case (te1, ff) => ff match{
        case FunctionType(tParams, domain, range) =>
          if(domain.length != args.length)
            FailureR(s"Expected ${domain.length} arguments, found "+args.length)
          else{
            // Create fresh type variables to replace tParams in domain and range
            val (te2, domain1, range1) = 
              subTypeParams(te1.newScope, tParams, domain, range)
              // subTypeParams(te1, tParams, domain, range)
            // Generate a new name, and bind it to range1 in the environment;
            // then unify the types of args with domain1, so the new name gets
            // updated to the appropriate return type.
            val name = newName(); val te3 = te2 + (name, range1)
            typeCheckListUnify(te3, args, domain1).map{ te4 => 
              Ok((te4.endScope, te4(name)))  // extract type of name
            }
          } 
        case _ => FailureR("Non-function applied as function")
      }}.lift(exp, true)
    // Block
    case BlockExp(stmts, e) => 
      // Create a new scope for this block, but return to the outer scope at
      // the end.
      dtc.typeCheckDeclList(typeEnv.newScope, stmts).map{ te1 => 
        typeCheckAndClose(te1, e).map{ case (te2, te) => Ok((te2.endScope, te)) }
      }.lift(exp)
      // Typed expressions
    case TypedExp(e, t) => 
      // Check t is not an unknown type parameter (or typo).
      if(t match{ case TypeParam(n) => !typeEnv.contains(n); case _ => false })
        FailureR(s"Type parameter ${t.asString} not in scope").lift(exp,true)
      else typeCheckUnify(typeEnv, e, t).lift(exp)
  } // end of typeCheck

  // ========= Unification, and closing.

  /** Typecheck exp, and unify with eType. */
  def typeCheckUnify(typeEnv: TypeEnv, exp: Exp, eType: TypeT)
      : Reply[TypeCheckRes] =
    typeCheck(typeEnv, exp).map{ case (te1,t) =>
      unify(te1, t, eType).lift(exp, true) // add line number here
    }
 
  // /** Check that all UntypedCellExps have been given a concrete type. */
  // def close(typeEnv: TypeEnv, t: TypeT): Reply[TypeCheckRes] = {
  //   val untypedCells = typeEnv.getUntypedCells
  //   if(untypedCells.isEmpty) Ok(typeEnv.removeUntypedCells, t)
  //   else{
  //     val s = if(untypedCells.length > 1) "s" else ""
  //     FailureR(
  //       s"Couldn't find type$s for cell expression$s "+
  //         untypedCells.map(_.getExtent.asString).mkString(", ")
  //     )
  //   }
  // }

  /** Typecheck exp in typeEnv, and ensure all UntypedCellExps have been given a
    * concrete type. */
  def typeCheckAndClose(typeEnv: TypeEnv, exp: Exp): Reply[TypeCheckRes] = 
    typeCheck(typeEnv, exp).map{ case (te1, t) => close(te1, t).lift(exp,true) }

  /** Typecheck exp, and unify with eType, and ensure all UntypedCellExps have
    * been given a concrete type. */
  def typeCheckUnifyAndClose(typeEnv: TypeEnv, exp: Exp, eType: TypeT)
      : Reply[TypeCheckRes] =
    typeCheck(typeEnv, exp).map{ case (te1,t1) =>
      unify(te1, t1, eType).map{ case (te2,t2) => close(te2, t2) }
    }.lift(exp, true) // add line number here

  // ===== Lists

  /** Typecheck exps, unifying all their types with t.  Return an appropriate
    * ListType if successful.  Used in typechecking a ListLiteral, so ensure
    * all elements have the same type. */
  private 
  def typeCheckListSingleType(typeEnv: TypeEnv, exps: List[Exp], t: TypeT)
      : Reply[(TypeEnv, ListType)] = 
    if(exps.isEmpty) Ok((typeEnv, ListType(t)))
    else typeCheckUnify(typeEnv, exps.head, t).map{ case (te1,t1) =>
      // Try to unify types of remainder with t1
      typeCheckListSingleType(te1, exps.tail, t1)
    }
  // Note: this traverses the list from left to right, which makes for more
  // natural error messages when type checking fails.

  // ===== Type checking function applications

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
    else typeCheck(typeEnv, es.head).map{ case (te1,t1) => 
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
}

// =======================================================

/** Type checker for binary operations. */
object BinOpTypeChecker{

}

// =======================================================

class BinOpTypeChecker(etc: ExpTypeCheckerT){
  import Unification.unify
  import ExpTypeChecker.{TypeCheckRes,close} // ,typeCheck,typeCheckUnify,close}

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
      : Reply[TypeCheckRes] =
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
