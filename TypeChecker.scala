package spreadsheet

/** The type checker. */
object TypeChecker{
  import TypeVar.TypeID // Type variables (Ints)
  import NameExp.Name // Names of identifiers (Strings)
  import TypeT.NumTypes // = List(IntType, FloatType) 
  import TypeParam.TypeParamName // Names of type parameters (Strings)
  import Unification.unify

  /** The next type identifier to use. */
  private var next = 0

  /** Get a new type identifier. */
  private def nextTypeID() : TypeID = TypeVar.getNext()

  private var nextNameIx = 0

  /** Get a new Name. */
  private def newName() : Name = { nextNameIx += 1; "%"+nextNameIx } 

  /** Make a FailureR: expected `eType` found `fType` in `exp`. */
  private def mkErr(eType: TypeT, fType: TypeT, exp: Exp) = {
    val source = exp.getExtent.asString
    FailureR(s"Expected ${eType.asString}, found "+fType.asString+
      s"\n\tin $source")
  }

  /** Contents of the result of a successful call to typeCheck. */
  type TypeCheckRes = (TypeEnv,TypeT)

  /** Typecheck expression `exp` in type environment `typeEnv`.
    * @return a Reply, if successful, the updated type environment and the 
    * type of exp. */
  private def typeCheck(typeEnv: TypeEnv, exp: Exp)
      : Reply[TypeCheckRes] = exp match{
    case NameExp(n) => typeEnv.get(n) match{
      case Some(t) => Ok((typeEnv,t))
      case None => FailureR("Name not found").lift(exp, true)
    }

    case IntExp(v) => Ok((typeEnv,IntType))
    case FloatExp(v) => Ok((typeEnv,FloatType))
    case BoolExp(v) => Ok((typeEnv,BoolType))
    case StringExp(st) => Ok((typeEnv,StringType))
    case RowExp(row) => Ok((typeEnv, RowType))
    case ColumnExp(column) => Ok((typeEnv, ColumnType))

    case BinOp(left, op, right) =>
      def idT(t: TypeT) = t
      // Create a triple: (1) an updated type environment; (2) the expected
      // type of the first argument; (3) a function to create the expected
      // type of the second argument from the type of the first argument; (4)
      // a function to create the type of the result from the type of the
      // second argument.  This represents that op has type (t,t) => mkRes(t),
      // where te captures type constraints on t.
      val (te, t, mk2nd, mkRes) = op match{
        case "+" | "-" | "*" | "/" =>   // Num a => (a,a) -> a
          val typeId = nextTypeID()
          val te = typeEnv + (typeId, MemberOf(NumTypes))
          (te, TypeVar(typeId), idT _, (t1: TypeT) => t1)
        case "==" | "!=" =>                    // Eq a => (a,a) -> Boolean
          val typeId = nextTypeID()
          val te = typeEnv + (typeId, EqTypeConstraint)
          (te, TypeVar(typeId), idT _, (_: TypeT) => BoolType)
        case "&&" | "||" =>             // (BoolType, BoolType) -> BoolType
          (typeEnv, BoolType, idT _, (_:TypeT) => BoolType)
        case "<=" | "<" | ">=" | ">" => // Num a => (a,a) -> a
          val typeId = nextTypeID()
          val te = typeEnv + (typeId, MemberOf(NumTypes)) 
          (te, TypeVar(typeId), idT _, (_: TypeT) => BoolType)
        case "::" =>  //  (a, ListType(a)) => ListType(a)
          val typeId = nextTypeID()
          val te = typeEnv + (typeId, AnyTypeConstraint)
          (te, TypeVar(typeId), ListType(_), idT _)
      }
      // Below t is the expected type of args; tl is the unification of t with
      // the type of left; tr is the unification of tl with the type of r; and
      // mkRes(tr) is the type of the result.
      typeCheckUnify(te, left, t).map{ case (te2, tl) =>
        typeCheckUnify(te2, right, mk2nd(tl)).map{ case (te3, tr) => 
          Ok((te3, mkRes(tr)))
        }
      }.lift(exp)

    case ce @ CellExp(column, row) => 
      typeCheck(typeEnv, column).mapOrLift(exp, { case (te1, tc) => 
        if(tc != ColumnType) mkErr(ColumnType, tc, column)
        else typeCheck(te1, row).mapOrLift(exp, { case (te2, tr) => 
          if(tr != RowType) mkErr(RowType, tr, row)
          else{
            // Associate type identifier with this read.
            val typeId = nextTypeID(); val typeVar = TypeVar(typeId); 
            ce.setType(typeVar)
            Ok((typeEnv.addCellConstraint(typeId, ce), typeVar))
          }
        })
      }).lift(exp)

    case IfExp(test, thenClause, elseClause) =>
      typeCheckUnify(typeEnv, test, BoolType).map{ case (te1, bt) =>
        assert(bt == BoolType)
        typeCheck(te1, thenClause).map{ case (te2,t1) =>
          typeCheckUnify(te2, elseClause, t1)
        }
      }.lift(exp)

    case ListLiteral(elems) => 
      if(elems.isEmpty){
        // Associate type identifier with this list.
        val typeId = nextTypeID()
        Ok((typeEnv + (typeId, AnyTypeConstraint), ListType(TypeVar(typeId))))
      }
      else typeCheck(typeEnv, elems.head).map{ case (te1, t1) =>
        // Try to unify types of remainder with t1
        typeCheckListSingleType(te1, elems.tail, t1).mapOrLift(exp, {
          case (te1, t) => Ok((te1, ListType(t)))
        })
      }

    case FunctionApp(f, args) => 
      typeCheck(typeEnv, f).map{ case (te1, ff) => ff match{
        case FunctionType(tParams, domain, range) =>
          if(domain.length != args.length)
            FailureR(s"Expected ${domain.length} arguments, found "+
              args.length).lift(exp, true)
          else{
            // Create fresh type variables to replace tParams in domain and range
            var updates = List[(TypeParamName, TypeVar)]();
            var constraints = List[(TypeID, StoredTypeConstraint)]()
            for((p,c) <- tParams){
              val tId = nextTypeID(); updates ::= (p, TypeVar(tId))
              constraints ::= (tId,c)
            }     
            // The instantiated domain and range
            val (domain1,range1) = Substitution.remapBy(updates, domain, range)
            val te2 = te1.addConstraints(constraints)
            // We generate a new name, and bind it to range1 in the environment.
            // Then unify the types of args with domain1, so the new name gets
            // updated to the appropriate return type.
            val name = newName(); val te3 = te2 + (name, range1)
            typeCheckListUnify(te3, args, domain1).mapOrLift(exp, { te4 =>
              Ok((te4-name, te4(name)))  // extract type of name
            })
          }

        case _ => FailureR("Non-function applied as function").lift(exp, true)
      }}

    case BlockExp(stmts, e) => 
      // We create a new scope for this block, but return to the outer scope
      // at the end.
      typeCheckStmtList(typeEnv.newScope, stmts).map{ te2 => 
        typeCheck(te2, e).map{ case (te3, te) => Ok((te3.endScope, te)) }
      }
  }

  /** Typecheck exp, and unify with eType. */
  private def typeCheckUnify(typeEnv: TypeEnv, exp: Exp, eType: TypeT)
      : Reply[(TypeEnv, TypeT)] =
    typeCheck(typeEnv, exp).map{ case (te1,t) =>
      unify(te1, t, eType).lift(exp, true) // add line number here
    }

  /** Typecheck exps, unifying all their types with t.  Used in typechecking a
    * ListLiteral, so ensure all elements have the same type. */
  private 
  def typeCheckListSingleType(typeEnv: TypeEnv, exps: List[Exp], t: TypeT)
      : Reply[(TypeEnv, TypeT)] = 
    if(exps.isEmpty) Ok((typeEnv, t))
    else typeCheckUnify(typeEnv, exps.head, t).map{ case (te1,t1) =>
      // Try to unify types of remainder with t1
      typeCheckListSingleType(te1, exps.tail, t1)
    }
  // Note: this traverses the list from left to right, which makes for more
  // natural error messages when type checking fails.

  /** Type check each element of es, unifying its type with the corresponding
    * element of ts. 
    * Pre: es.length == ts.length.
    * Used to check parameters of a function application against the expected 
    * types.  */
  private 
  def typeCheckListUnify(typeEnv: TypeEnv, es: List[Exp], ts: List[TypeT])
      : Reply[TypeEnv] = 
    if(es.isEmpty){ assert(ts.isEmpty); Ok(typeEnv) }
    else typeCheckUnify(typeEnv, es.head, ts.head).map{ case (te2, t11) =>
      typeCheckListUnify(te2, es.tail, ts.tail)
    }

  // ==================================================================
  // Statements

  /** Typecheck the statement `stmt`. 
    * If successful, return the resulting type environment. */
  private 
  def typeCheckStmt(typeEnv: TypeEnv, stmt: Statement): Reply[TypeEnv] = 
    stmt match{
      case Directive(cell, expr) => 
        typeCheck(typeEnv, expr).map{ case (te1, t) => 
          typeCheck(te1, cell).map{ case(te2, TypeVar(tId)) => 
            assert(te2(tId) == MemberOf(TypeT.CellTypes))
            // Unify t with above constraint: expr should give a cell value
            unify(te2, t, TypeVar(tId)).map{ case (te3, tt) => 
              Ok(te3) 
            }.lift(expr, true)
          }
        }.lift(stmt)

      case ValueDeclaration(name, exp) => 
        typeCheck(typeEnv, exp).mapOrLift(stmt, { case (te1, t) => 
          Ok(te1 + (name, t))
        })

      case FunctionDeclaration(name, tparams, params, rt, body) =>
        // name should already be bound to an appropriate FunctionType
        require(typeEnv(name) == FunctionType(tparams, params.map(_._2), rt))
        // Check names of params, tparams are disjoint
        (findRepetition(params.map(_._1)) match{
          case Some(p) => FailureR(s"Repeated parameter $p")
          case None => findRepetition(tparams.map(_._1)) match{
            case Some(tp) => FailureR(s"Repeated type parameter $tp")
            case None => 
              // Create a new scope, and extend with params and tparams
              val te1 = (typeEnv.newScope ++ params).addTypeParams(tparams)
              // Type parameters used in params
              val usedTParams = params.flatMap(_._2.typeParams)
              val invalidTParams = usedTParams.filter(p => !te1.hasTypeParam(p))
              if(invalidTParams.nonEmpty)
                FailureR(s"Unknown type(s): "+invalidTParams.mkString(", "))
              else
                // Typecheck body, and make sure return type matches rt
                typeCheckUnify(te1, body, rt).map{ case (te3, tt) =>
                  Ok(te3.endScope) // back to the old scope
                }
          }
        }).lift(stmt)

    } // end of "stmt match"

  /** Find a repeated value in xs, if there is one. */
  private def findRepetition[A](xs: List[A]): Option[A] = 
    if(xs.isEmpty) None
    else if(xs.tail.contains(xs.head)) Some(xs.head)
    else findRepetition(xs.tail)

  /** Typecheck stmts in environment typeEnv. */
  private def typeCheckStmtList(typeEnv: TypeEnv, stmts: List[Statement])
      : Reply[TypeEnv] = {
    // Extend typeEnv on assumption all FunctionDeclarations are correctly
    // typed.
    val updates = (
      for(FunctionDeclaration(name, tparams, params, rt, body) <- stmts) yield 
        name -> FunctionType(tparams, params.map(_._2), rt)
    )
    val typeEnv1 = typeEnv ++ updates 
    typeCheckStmtList1(typeEnv1, stmts) 
                                          // TODO: check names disjoint
  }

  /** Typecheck stmts in environment typeEnv.  All names of functions should
    * already be bound to the claimed types. */ 
  private def typeCheckStmtList1(typeEnv: TypeEnv, stmts: List[Statement])
      : Reply[TypeEnv] =
    if(stmts.isEmpty) Ok(typeEnv)
    else typeCheckStmt(typeEnv, stmts.head).map{ te1 => 
      typeCheckStmtList1(te1, stmts.tail)
    }

  /** Typecheck stmts. */
  def apply(stmts: List[Statement]): Reply[TypeEnv] =
    typeCheckStmtList(TypeEnv(), stmts)

  // ========= Testing

  val outer = this

  /** Test hooks, to give TypeCheckerTest access to private operations. */
  object TestHooks{
    val typeCheck = outer.typeCheck _
    val typeCheckStmtList = outer.typeCheckStmtList _
  }

}
