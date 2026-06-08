package spreadsheet

import TypeVar.{TypeID,nextTypeID} // Type variables (Ints)
import TypeParam.TypeParamName // Names of type parameters (Strings)
import NameExp.Name // Names of identifiers (Strings)
import TypeT.showList
import Substitution.substitute

/** The interface of DeclarationTypeChecker, as seen by ExpTypeChecker. */
trait TypeCheckerT{ 
  /** Type check stmts, returning the resulting type environment if
    * successful. 
    * @param topLevel is this at the top level, where operation declarations are 
    * allowed?  */
  def typeCheckStmtList(
    typeEnv: TypeEnv, stmts: List[Statement], topLevel: Boolean = false)
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

  /** Try to instantiate ft, which should be a FunctionType, with actual type
    * parameters atps. */
  private 
  def instantiate(typeEnv: TypeEnv, n: String, atps: List[TypeT], ft: TypeT)
      : TypeCheckRes = {
    val FunctionType(tps, domain, range) = ft
    if(tps.length == atps.length){
      // Update te so that an actual type parameter satisfies the
      // corresponding type constraint.
      def update(te: TypeEnv, pair: (TypeParameter, TypeT)): Reply[TypeEnv] = {
        val ((ftp,tc), atp) = pair
        def fail = FailureR(
          s"Actual type parameter ${atp.asString} does not satisfy "+
            s"type constraint ${tc.asString}")
        te.updateEnvToSatisfy(atp, tc, fail)
      }
      // Do this for all the type parameters
      Reply.fold(update _, typeEnv, tps.zip(atps)).map{ case te1 =>
        // Substitute formal type parameters with actual type parameters.
        val map = Map.from(tps.map(_._1).zip(atps))
        val range1 = substitute(map, range)
        val domain1 = domain.map(substitute(map,_))
        Ok((te1, FunctionType(List(), domain1, range1)))
      }
    }
    else FailureR(s"Wrong number of type parameters for function $n")
  }

  /** Get all possible types for `ne`, instantiating type parameters of
    * FunctionTypes appropriately (if possible).  Each type is combined with
    * the corresponding type environment, and its index in the overloading
    * list. */ 
  private def getAll(typeEnv: TypeEnv, ne: NameExp)
      : Reply[List[(TypeEnv,TypeT,Int)]] = {
    val NameExp(n, atps) = ne
    typeEnv.get(n) match{
      case None => FailureR(s"Name $n not found") 
      case Some(List()) => FailureR(s"Forward reference to name $n") 
      case Some(List(t)) => 
        if(atps.isEmpty) Ok(List((typeEnv,t,-1)))
        else t match{
          case ft: FunctionType => 
            instantiate(typeEnv, n, atps, ft) match{
              case Ok((te,t1)) => Ok(List((te,t1,-1)))
              case fail: FailureR => fail
            }
          case _ => FailureR(s"Type parameters applied to non-function $n")
        }

      case Some(ts) => 
        if(atps.isEmpty) Ok(ts.zipWithIndex.map{ case(t,i) => (typeEnv,t,i) }) 
        else{
          // Instantiate type parameters in each
          val prs0 = ts.map(t => 
            instantiate(typeEnv, n, atps, t.asInstanceOf[FunctionType]))
          // Select successes, and combine with indices. 
          val prs = prs0.zipWithIndex.filter(
            _._1.isInstanceOf[Ok[(TypeEnv,TypeT)]])
          if(prs.nonEmpty) Ok(prs.map{case (Ok((te,t1)),i) => (te,t1,i)})
          // prs0.length > 1, so following case doesn't apply.
          // else if(prs0.length == 1) prs0.head.asInstanceOf[FailureR]
          else FailureR(s"Cannot resolve overloaded name $n with types\n"
            +showList(ts))
        }
    }
  }

  // ===== Type checking of expression

  /** Typecheck expression `exp` in type environment `typeEnv`.
    * @return a Reply, if successful, the updated type environment and the 
    * type of exp. */
  def typeCheck(typeEnv: TypeEnv, exp: Exp): TypeCheckRes = exp match{
    case ne @ NameExp(n, atps) => getAll(typeEnv, ne).map{
      case List() => sys.error(s"typeCheck $ne") // shouldn't happen
      case List((te,t,i)) => ne.setIndex(i); Ok((te,t))
      case _ => FailureR(s"Cannot resolve overloaded name $n")
    }.lift(exp, true)

    case TypedExp(ne @ NameExp(n, atps), t) => getAll(typeEnv, ne).map{ _ match{
      case List((te1,t1,i)) => ne.setIndex(i); unify(te1, t1, t)
      case List() => ??? // shouldn't happen
      case triples => 
        val filtered = triples.filter(_._2 == t)
        if(filtered.isEmpty) FailureR(s"Cannot find matching type for name $n")
        else if(filtered.length == 1){
          val (te1,`t`,index) = filtered.head; ne.setIndex(index); Ok((te1,t))
        }
        else FailureR(s"Ambiguous use of overloaded name $n")
    } }.lift(exp, true)

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

    // Conditionals
    case IfExp(test, thenClause, elseClause) =>
      typeCheckUnify(typeEnv, test, BoolType).map{ case (te1, bt) =>
        assert(bt == BoolType)
        typeCheck/*AndClose*/(te1, thenClause).map{ case (te2,t1) =>
          TypeChecker0.closeCells(te2,t1).map{ case (te3,t2) => 
            typeCheckUnifyAndClose(te3, elseClause, t2)
          }
        }
        // Note: don't close in "then" branch, to allow expressions like "[]",
        // where the type is resolved by the "else" branch.
      }.lift(exp)

    // Typed cell expressions
    case ce @ CellExp(column, row, theType) =>
      def checkType(te: TypeEnv) = theType match{
        case TypeParam(tp) => 
          def fail = FailureR(s"Expected CellType, found $tp")
          te.updateEnvToSatisfy(theType, CellTypeConstraint, fail).map{
            case te2 => Ok(te2, theType)
          }
        case _ => Ok(te, theType)
      }
      checkCellRead(typeEnv, column, row, checkType).lift(exp)
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
      // List comprehensions
    case ListComprehension(e, qs) => 
      checkQualifiers(typeEnv, qs).map{ te1 =>
        typeCheck(te1, e).map{ case (te2,t) => Ok(te2, ListType(t)) }
      }.lift(exp)
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
    case fa @ FunctionApp(ne @ NameExp(fn, _), args) => getAll(typeEnv, ne).map{
      case List() =>  sys.error(s"typeCheck $fa") // shouldn't happen
      case List((te1,t,i)) => 
        ne.setIndex(i)
        fatc.checkFunctionApp(te1, t, args).lift(fa, true) 
      case triples => 
        // Try the instance `triple`.
        def tryPair(triple: (TypeEnv,TypeT,Int)): (TypeCheckRes,Int) = {
          val (te1,t1,i) = triple
          (fatc.checkFunctionApp(te1, t1.asInstanceOf[FunctionType], args), i)
        }
        // Find successes; there should be only one
        val results = triples.map(tryPair)
        results.filter(_._1.isOk) match{
          case List() => results.head._1 // Return first error here.
              // FailureR(s"Cannot find matching type for name $fn")
          case List((ok,i)) => ne.setIndex(i); ok
          case _ =>  FailureR(s"Ambiguous application of overloaded name $fn")
        }
    }
    // General function applications
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
        // Note: necessarily ts.length <= Tuple.MaxArity here.
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
