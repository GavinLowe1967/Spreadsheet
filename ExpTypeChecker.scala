package spreadsheet

import TypeVar.{TypeID,nextTypeID} // Type variables (Ints)
import TypeParam.TypeParamName // Names of type parameters (Strings)
import NameExp.Name // Names of identifiers (Strings)
import TypeT.showList

/** The interface of DeclarationTypeChecker, as seen by ExpTypeChecker. */
trait TypeCheckerT{
  /** Type check decls, returning the resulting type environment if
    * successful. */
  // def typeCheckDeclList(typeEnv: TypeEnv, decls: List[Declaration])
  //     : Reply[TypeEnv]
 
  /** Type check stmts, returning the resulting type environment if
    * successful. */
  def typeCheckStmtList(typeEnv: TypeEnv, stmts: List[Statement])
      : Reply[TypeEnv] 
}
 
// =======================================================

// Note: a single ExpTypeChecker object is created, in DeclarationTypeChecker. 

/** Type checker for expressions. */
class ExpTypeChecker(dtc: TypeCheckerT) extends ExpTypeCheckerT{
  import FunctionType.TypeParameter // (TypeParamName, TypeParamConstraint)
  import Unification.unify
  import TypeChecker0.{TypeCheckRes,close}

  /** The object used to typecheck binary operations. */
  private val botc = new BinOpTypeChecker(this)

  /** Object used in type checking cell reads. */
  private val crtc = new CellReadTypeChecker(this)

  private val checkCellRead = crtc.checkCellRead _

  /** Object used in typechecking function applications. */
  private val fatc = new FunctionAppTypeChecker(this)

  // ===== Type checking of expression

  /** Typecheck expression `exp` in type environment `typeEnv`.
    * @return a Reply, if successful, the updated type environment and the 
    * type of exp. */
  def typeCheck(typeEnv: TypeEnv, exp: Exp): TypeCheckRes = exp match{
    case NameExp(n) => (typeEnv.get(n) match{
      case None => FailureR(s"Name $n not found") 
      case Some(List()) => FailureR(s"Forward reference to name $n") 
      case Some(List(t)) => Ok((typeEnv,t))
      case Some(ts) => 
        FailureR(s"Cannot resolve overloaded name $n with types\n"+showList(ts))
    }).lift(exp, true)
    case TypedExp(ne @ NameExp(n), t) => (typeEnv.get(n) match{
      case None => FailureR(s"Name $n not found")
      case Some(List()) =>  FailureR(s"Forward reference to name $n")
      case Some(List(t1)) => unify(typeEnv, t1, t)
      case Some(ts) =>
        val index = ts.indexOf(t)
        if(index >= 0){ ne.setIndex(index); Ok((typeEnv,t)) } 
        else FailureR(s"Overloaded name $n with types\n"+showList(ts)+
          s"\nis not of type ${t.asString}")
    }).lift(exp, true)
    // Atomic types
    case IntExp(v) => Ok((typeEnv,IntType))
    case FloatExp(v) => Ok((typeEnv,FloatType))
    case BoolExp(v) => Ok((typeEnv,BoolType))
    case StringExp(st) => Ok((typeEnv,StringType))
    case UnitExp => Ok((typeEnv,UnitType))
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
        typeEnv, column, row, te => crtc.typeCheckBranches(te, branches)
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
      // Tuple literals
    case TupleLiteral(elems) => 
      if(elems.length > TupleType.MaxArity)
        FailureR(s"Tuple has more than maximum allowed number of components "+
          s"(${TupleType.MaxArity})").lift(exp,true)
      else 
        typeCheckList(typeEnv, elems).map{ case (te1,ts) => 
          Ok(te1, TupleType(ts)) 
        }.lift(exp)

    // Application of function name; allow overloading here
    case fa @ FunctionApp(NameExp(fn), args) => (typeEnv.get(fn) match{
      case None => FailureR(s"Name $fn not found").lift(exp, true) 
      case Some(List()) => 
        FailureR(s"Forward reference to name $fn").lift(exp, true)
      case Some(List(t)) => 
        // println(s"$fn $args $t")
        fatc.checkFunctionApp(typeEnv, t, args) .lift(fa, true)
        //   case ok @ Ok(_) => ok
        //   case f @ FailureR(err) => println(s"**$err"); f.lift(fa,true)
        // }
      case Some(ts) => 
        assert(ts.nonEmpty && ts.forall(_.isInstanceOf[FunctionType])) 
        val ts1 = ts.map(_.asInstanceOf[FunctionType]).toArray
        fatc.findFunctionApp(typeEnv, fa, ts1) // Note: don't lift here.
    })
    // Function applications
    case FunctionApp(f, args) => 
      typeCheck(typeEnv, f).lift(exp).map{ case (te1, ff) =>
        fatc.checkFunctionApp(te1, ff, args).lift(exp, true)
      }//.lift(exp, true)
    // Block
    case BlockExp(stmts, e) => 
      // Create a new scope for this block, but return to the outer scope at
      // the end.
      dtc.typeCheckStmtList(typeEnv.newScope, stmts).map{ te1 => 
        if(e != null) typeCheckAndClose(te1, e).map{ 
          case (te2, te) => Ok((te2.endScope, te)) 
        }
        else Ok((te1.endScope, UnitType))
      }.lift(exp)
      // Typed expressions
    case ListComprehension(e, qs) => 
      checkQualifiers(typeEnv, qs).map{ te1 =>
        typeCheck(te1, e).map{ case (te2,t) => Ok(te2, ListType(t)) }
      }.lift(exp)
    case TypedExp(e, t) => 
      // Check t is not an unknown type parameter (or typo).
      if(t match{ case TypeParam(n) => !typeEnv.contains(n); case _ => false })
        FailureR(s"Type parameter ${t.asString} not in scope").lift(exp,true)
      else typeCheckUnify(typeEnv, e, t).lift(exp)
  } // end of typeCheck

  // ========= Unification, and closing.

  /** Typecheck exp, and unify with eType. */
  def typeCheckUnify(typeEnv: TypeEnv, exp: Exp, eType: TypeT): TypeCheckRes =
    typeCheck(typeEnv, exp).map{ case (te1,t) =>
      unify(te1, t, eType).lift(exp, true) // add line number here
    }
 
  /** Typecheck exp in typeEnv, and ensure all UntypedCellExps have been given a
    * concrete type. */
  def typeCheckAndClose(typeEnv: TypeEnv, exp: Exp): TypeCheckRes = 
    typeCheck(typeEnv, exp).map{ case (te1, t) => close(te1, t).lift(exp,true) }

  /** Typecheck exp, and unify with eType, and ensure all UntypedCellExps have
    * been given a concrete type. */
  def typeCheckUnifyAndClose(typeEnv: TypeEnv, exp: Exp, eType: TypeT)
      : TypeCheckRes =
    typeCheck(typeEnv, exp).map{ case (te1,t1) =>
      unify(te1, t1, eType).map{ 
        case (te2,t2) => close(te2, t2)
      }.lift(exp, true) // add line number here
    }

  // ========== Lists

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

  /** Typecheck exps.  If successful, return the resulting type environment and
    * the list of types.*/
  def typeCheckList(typeEnv: TypeEnv, exps: List[Exp])
      : Reply[(TypeEnv, List[TypeT])] =
    if(exps.isEmpty) Ok(typeEnv, List[TypeT]())
    else typeCheck(typeEnv, exps.head).map{ case (te1, t1) =>
      typeCheckList(te1, exps.tail).map{ case(te2, ts) => Ok(te2, t1::ts) }
    }

  // ========= Qualifiers

  /** Try to bind `pat` to `t`, giving updated TypeEnv. */
  def bindNames(typeEnv: TypeEnv, pat: Pattern, t: TypeT)
      : Reply[TypeEnv] = pat match{
    case NamePattern(name) => Ok(typeEnv + (name, t))
    case TuplePattern(pats) => t match{
      case TupleType(ts) if pats.length == ts.length => 
        // Recurse on corresponding patterns and types
        def f(te: TypeEnv, pair: (Pattern, TypeT)): Reply[TypeEnv] = 
          bindNames(te, pair._1, pair._2)
        Reply.fold(f _, typeEnv, pats.zip(ts)).lift(pat)
      case _ => 
        FailureR(s"Cannot bind pattern to type ${t.asString}").lift(pat,true)
    }
  }


  /** Typecheck qs, returning an updated TypeEnv if successful. */
  def checkQualifiers(typeEnv: TypeEnv, qs: List[Qualifier]): Reply[TypeEnv] = 
    Reply.fold(checkQualifier, typeEnv, qs)

  /** Typecheck q, returning an updated TypeEnv if successful. */
  private 
  def checkQualifier(typeEnv: TypeEnv, q: Qualifier): Reply[TypeEnv] = q match{
    case Generator(pattern, list) =>   // list should be a ListType
      typeCheckAndClose(typeEnv, list).map{
        //case (te1, ListType(t)) => Ok(te1+(name,t)) // bind name
        case (te1, ListType(t)) => bindNames(te1, pattern, t) // bind names
        case (_, t1) =>
          FailureR(s"Expected List, found ${t1.asString}").lift(list,true)
      }.lift(q)
    case Filter(test) =>
      typeCheckUnifyAndClose(typeEnv, test, BoolType).map{
        case (te, BoolType) => Ok(te)
      }
  }
  
}
