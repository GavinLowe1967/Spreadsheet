package spreadsheet

object TypeChecker{
  import TypeVar.TypeID // Type variables (Ints)
  import NameExp.Name // Names of identifiers (Strings)

  /** The next type identifier to use. */
  private var next = 0

  /** Get a new type identifier. */
  private def nextTypeID() : TypeID = { next += 1; next-1 }

  import EvaluationTypeChecker.mkFailure

  /** Replace tId by t in typeEnv, if it is consistent with the constraints in
    * typeEnv. Otherwise return fail. */
  private def replaceInTypeEnv(
    typeEnv: TypeEnv, tId: TypeID, t: TypeT, fail: => FailureR)
      : Reply[(TypeEnv, TypeT)] =
    if(typeEnv(tId).satisfiedBy(t)) Ok(typeEnv.replace(tId, t), t)
    else fail

  /** Try to unify t1 and t2.  If successful, return updated typeEnv and unified
    * type.  t2 is expected to be the "expected" type, and t1 the type that is
    * being matched against it. */
  def unify(typeEnv: TypeEnv, t1: TypeT, t2: TypeT): Reply[(TypeEnv, TypeT)] = {
    def fail = mkFailure(typeEnv, t1, t2)
    if(t1 == t2) Ok(typeEnv, t1)
    else (t1,t2) match{
      case (TypeVar(tId1), TypeVar(tId2)) => // Both TypeVars: find intersection
        typeEnv(tId1).intersection(typeEnv(tId2)) match{
          case EmptyTypeConstraint =>  fail
          case SingletonTypeConstraint(t) =>
            val newTypeEnv = typeEnv.replace(tId1, t).replace(tId2, t)
            Ok(newTypeEnv, t)
          case cc : StoredTypeConstraint =>
            // replace t2 by t1, with constraint cc
            Ok(typeEnv.replace(tId2, t1) + (tId1, cc), t1)
                                                      // TODO: test with ts1!=ts2
        }

      case (TypeVar(tId1), _) => // t2 a concrete type.  Try to replace t1 by t2
        replaceInTypeEnv(typeEnv, tId1, t2, fail)
        
      case ( _, TypeVar(tId2)) =>           // Try to replace t2 by t1
        replaceInTypeEnv(typeEnv, tId2, t1, fail)

      case (_,_) => fail
    }
  }

  /** Try to unify t1 and t2, at runtime.  Do not update any typing in cells
    * (within typeEnv.replace).  Pre: t1 is a concrete type (but t2 might be a
    * TypeVar). */
  // def unifyEvalTime(typeEnv: TypeEnv, t1: TypeT, t2: TypeT)
  //     : Reply[(TypeEnv, TypeT)] = {
  //   def fail = FailureR(
  //     s"Expected "+typeEnv.showType(t2)+", found "+typeEnv.showType(t1))
  //   require(!t1.isInstanceOf[TypeVar])
  //   t2 match{
  //     case TypeVar(tId2) => replaceInTypeEnv(typeEnv, tId2, t1, fail)
  //       // if(typeEnv(tId2).satisfiedBy(t1)) 
  //       //   Ok(typeEnv.replaceEvalTime(tId2, t1), t1)
  //       // else fail

  //     case _ => if(t1 == t2) Ok(typeEnv, t2) else fail
  //   }
  // }

  /** Make a FailureR: expected `eType` found `fType` in `exp`. */
  private def mkErr(eType: TypeT, fType: TypeT, exp: Exp) = {
    val source = exp.getExtent.asString
    FailureR(s"Expected ${eType.asString}, found "+fType.asString+
      s"\n\tin $source")
  }

  import TypeT.NumTypes // = List(IntType, FloatType) 

  /** Contents of the result of a successful call to typeCheck. */
  type TypeCheckRes = (TypeEnv,TypeT)

  /** Typecheck expression `exp` in type environment `typeEnv`. */
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
      // Create a triple: (1) an updated type environment; (2) the expected
      // type of the arguments; (3) a function to create the type of the
      // result from the type of the arguments.  This represents that op has
      // type (t,t) => mkRes(t), where te captures type constraints on t. 
      val (te, t, mkRes) = op match{
        case "+" | "-" | "*" | "/" =>   // Num a => (a,a) -> a
          val typeId = nextTypeID()
          val te = typeEnv + (typeId, MemberOf(NumTypes))
          (te, TypeVar(typeId), (t1: TypeT) => t1)
        case "==" | "!=" =>                    // Eq a => (a,a) -> Boolean
          val typeId = nextTypeID()
          val te = typeEnv + (typeId, EqTypeConstraint)
          (te, TypeVar(typeId), (_: TypeT) => BoolType)
        case "&&" | "||" =>             // (BoolType, BoolType) -> BoolType
          (typeEnv, BoolType, (_:TypeT) => BoolType)
        case "<=" | "<" | ">=" | ">" => // Num a => (a,a) -> a
          val typeId = nextTypeID()
          val te = typeEnv + (typeId, MemberOf(NumTypes)) 
          (te, TypeVar(typeId), (_: TypeT) => BoolType)
      }
      // Below t is the expected type of args; tl is the unification of t with
      // the type of left; tr is the unification of tl with the type of r; and
      // mkRes(tr) is the type of the result.
      typeCheckUnify(te, left, t).map{ case (te2, tl) =>
        typeCheckUnify(te2, right, tl).map{ case (te3, tr) => 
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
      require(elems.nonEmpty)                                 // IMPROVE
      typeCheckListSingleType(typeEnv, elems).mapOrLift(exp, { case (te1, t) =>
        // ll.setUnderlyingType(t); 
        Ok((te1, ListType(t)))
      })

    case FunctionApp(f, args) => 
      typeCheck(typeEnv, f).map{ case (te1, ff) => 
        ff match{
          case FunctionType(domain, range) => 
            // Check actual param types (args) match formal param types (domain)
            if(domain.length != args.length)
              FailureR(s"Expected ${domain.length} arguments, found "+
                args.length).lift(exp, true)
            else typeCheckListUnify(te1, args, domain).mapOrLift(exp, { te2 => 
              Ok((te2, range)) 
            })

          case _ => FailureR("Non-function applied as function").lift(exp, true)
        }
      }

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

  /** Typecheck exps, expecting each element to have the same type. */
  private def typeCheckListSingleType(typeEnv: TypeEnv, exps: List[Exp])
      : Reply[(TypeEnv, TypeT)] = {
    assert(exps.nonEmpty)                         // IMPROVE
    val head = exps.head; val tail = exps.tail
    typeCheck(typeEnv, head).map{ case(te1, t1) =>
      if(tail.isEmpty) Ok((te1, t1))
      else // typecheck tail, and unify with t1
        typeCheckListSingleType(te1, tail).map{ case (te2, t2) =>
          unify(te2, t1, t2).lift(head, true)
        }
    }
  }

  /** Type check each element of es, unifying its type with the corresponding
    * element of ts. 
    * Pre: es.length == ts.length, and each element of ts is a concrete type.
    * Used to check parameters of a function application against the expected 
    * types.  */
  private 
  def typeCheckListUnify(typeEnv: TypeEnv, es: List[Exp], ts: List[TypeT])
      : Reply[TypeEnv] = 
    if(es.isEmpty){ assert(ts.isEmpty); Ok(typeEnv) }
    else typeCheckUnify(typeEnv, es.head, ts.head).map{ case (te2, t11) =>
      assert(t11 == ts.head) // ts.head should be concrete
      typeCheckListUnify(te2, es.tail, ts.tail)
    }

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

      case FunctionDeclaration(name, params, rt, body) =>
        // name should already be bound to an appropriate FunctionType
        require(typeEnv(name) == FunctionType(params.map(_._2), rt))
        // Create a new scope, and extend with params
        val te1 = typeEnv.newScope ++ params
        // Typecheck body, and make sure return type matches rt
        typeCheckUnify(te1, body, rt).mapOrLift(stmt, { case (te3, tt) =>
          Ok(te3.endScope) // back to the old scope
        })

    } // end of "stmt match"

  /** Typecheck stmts in environment typeEnv. */
  private def typeCheckStmtList(typeEnv: TypeEnv, stmts: List[Statement])
      : Reply[TypeEnv] = {
    // Extend typeEnv on assumption all FunctionDeclarations are correctly
    // typed.
    val updates = (
      for(fd @ FunctionDeclaration(name, params, rt, body) <- stmts) yield 
        name -> FunctionType(params.map(_._2), rt)
    )
    val typeEnv1 = typeEnv ++ updates 
    typeCheckStmtList1(typeEnv1, stmts) 
                                          // TODO: check names disjoint
  }

  def apply(stmts: List[Statement]): Reply[TypeEnv] =
    typeCheckStmtList(TypeEnv(), stmts)

  /** Typecheck stmts in environment typeEnv.  All names of functions should
    * already be bound to the claimed types. */ 
  private def typeCheckStmtList1(typeEnv: TypeEnv, stmts: List[Statement])
      : Reply[TypeEnv] =
    if(stmts.isEmpty) Ok(typeEnv)
    else typeCheckStmt(typeEnv, stmts.head).map{ te1 => 
      typeCheckStmtList1(te1, stmts.tail)
    }

  val outer = this

  /** Test hooks, to give TypeCheckerTest access to private operations. */
  object TestHooks{
    val typeCheck = outer.typeCheck _
    val typeCheckStmtList = outer.typeCheckStmtList _
  }

}
