package spreadsheet

/** The type checker. */
object TypeChecker{
  import TypeVar.TypeID // Type variables (Ints)
  import NameExp.Name // Names of identifiers (Strings)
  import TypeT.NumTypes // = List(IntType, FloatType) 
  import TypeParam.TypeParamName // Names of type parameters (Strings)
  import FunctionType.TypeParameter // (TypeParamName, TypeParamConstraint)
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

  /** Make a FailureR: "Expected Int or Float...". */
  private def mkIntFloatErr(fType: TypeT, exp: Exp) = {
    val source = exp.getExtent.asString
    FailureR(s"Expected Int or Float, found "+fType.asString+
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
    // Atomic types
    case IntExp(v) => Ok((typeEnv,IntType))
    case FloatExp(v) => Ok((typeEnv,FloatType))
    case BoolExp(v) => Ok((typeEnv,BoolType))
    case StringExp(st) => Ok((typeEnv,StringType))
    case RowExp(row) => Ok((typeEnv, RowType))
    case ColumnExp(column) => Ok((typeEnv, ColumnType))
    // Binary operators  
    case BinOp(left, op, right) =>
      typeCheck(typeEnv, left).map{ case (te1, tl) =>
        typeCheck(te1, right).map{ case (te2, tr) =>
          op match{
            case "+" | "-" | "*" | "/" =>
              if(tl == IntType || tl == FloatType)
                if(tl == tr) Ok((te2, tl)) else mkErr(tl, tr, right)
              else mkIntFloatErr(tl, left)
            case "==" | "!=" => 
              if(te2.isEqType(tl))
                unify(te2, tr, tl).map{ case (te3,_) => Ok((te3,BoolType)) }
              else FailureR(s"Expected equality type, found $tl").lift(left)
            case "<=" | "<" | ">=" | ">" =>
              if(tl == IntType || tl == FloatType)
                if(tl == tr) Ok((te2, BoolType)) else mkErr(tl, tr, right)
              else mkIntFloatErr(tl, left)
            case "&&" | "||" =>
              if(tl == BoolType)
                if(tr == BoolType) Ok((te2, BoolType)) else mkErr(tl, tr, right)
              else mkErr(BoolType, tl, left)
            case "::" => 
              unify(te2, tr, ListType(tl))
            case "to" | "until" => 
              if(tl == IntType || tl == RowType || tl == ColumnType)
                if(tl == tr) Ok((te2, ListType(tl))) else mkErr(tl, tr, right)
              else
                FailureR(s"Expected Int, Row or Column, found "+tl.asString
                ).lift(left)
          } // end of op match
        }
      }.lift(exp)
    // Cell expressions
    case ce @ CellExp(column, row, theType) =>
      typeCheckUnify(typeEnv, column, ColumnType).map{ case (te1, ColumnType) => 
        typeCheckUnify(te1, row, RowType).map{ case (te2, RowType) =>
          Ok(te2, theType)
        }
      }.lift(exp)
    // Conditionals
    case IfExp(test, thenClause, elseClause) =>
      typeCheckUnify(typeEnv, test, BoolType).map{ case (te1, bt) =>
        assert(bt == BoolType)
        typeCheck(te1, thenClause).map{ case (te2,t1) =>
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
              subTypeParams(te1, tParams, domain, range)
            // Generate a new name, and bind it to range1 in the environment;
            // then unify the types of args with domain1, so the new name gets
            // updated to the appropriate return type.
            // println(FunctionApp(f,args)); 
            // println(s"tParams = $tParams; domain1 = $domain1")
            val name = newName(); val te3 = te2 + (name, range1)
            typeCheckListUnify(te3, args, domain1).map{ te4 => 
              Ok((te4-name, te4(name)))  // extract type of name
            }
          } 
        case _ => FailureR("Non-function applied as function")
      }}.lift(exp, true)
    // Block
    case BlockExp(stmts, e) => 
      // Create a new scope for this block, but return to the outer scope at
      // the end.
      typeCheckStmtList(typeEnv.newScope, stmts).map{ te1 => 
        typeCheck(te1, e).map{ case (te2, te) => Ok((te2.endScope, te)) }
      }
  }

  /** Typecheck exp, and unify with eType. */
  private def typeCheckUnify(typeEnv: TypeEnv, exp: Exp, eType: TypeT)
      : Reply[(TypeEnv, TypeT)] =
    typeCheck(typeEnv, exp).map{ case (te1,t) =>
      unify(te1, t, eType).lift(exp, true) // add line number here
    }

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

  /** If t is a FunctionType, make an instance of it, replacing each type
    * parameter by a fresh type variable, and add suitable constraints to
    * typeEnv. */
  private def mkInstance(typeEnv: TypeEnv, t: TypeT): (TypeEnv, TypeT) = t match{
    case FunctionType(tParams, domain, range) => 
      val (te, domain1, range1) = subTypeParams(typeEnv, tParams, domain, range)
      (te, FunctionType(List(), domain1, range1))
    case _ => (typeEnv, t)
  }

  // ============================================================= Statements

  /** Typecheck the statement `stmt`. 
    * If successful, return the resulting type environment. */
  private 
  def typeCheckStmt(typeEnv: TypeEnv, stmt: Statement): Reply[TypeEnv] = 
    stmt match{
      case Directive(column, row, expr) =>
        typeCheck(typeEnv, expr).map{ case (te1, t) => 
          if(! TypeT.CellTypes.contains(t))
            FailureR(s"Expected cell type, found ${t.asString} in ${expr}")
          else
            typeCheckUnify(te1, row, RowType).map{ case(te2, RowType) =>
              typeCheckUnify(te2, column, ColumnType).map{
                case(te3, ColumnType) => Ok(te3)
              }
            }
        }.lift(stmt)

      case ValueDeclaration(name, exp) => 
        typeCheck(typeEnv, exp).mapOrLift(stmt, { case (te1, t) => 
          Ok(te1 + (name, t))
        })

      case FunctionDeclaration(name, tparams, params, rt, body) =>
        // name should already be bound to an appropriate FunctionType, by
        // typeCheckStmtList
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
              val usedTParams: List[TypeParamName] = 
                params.flatMap(_._2.typeParams) ++ rt.typeParams
              val invalidTParams = usedTParams.filter(p => !te1.hasTypeParam(p))
              if(invalidTParams.nonEmpty)
                FailureR(s"Unknown type(s): "+invalidTParams.mkString(", "))
              else
                // Typecheck body, and make sure return type matches rt
                typeCheckUnify(te1, body, rt).map{ case (te3, tt) =>
                  Ok(te3.endScope) // back to the old scope
                }
// FIXME: if any of tparams gets bound to a TypeVar, update in typeEnv(name)
          }
        }).lift(stmt)

      case ForStatement(binders, stmts) => 
        checkFor(typeEnv.newScope, binders, stmts).lift(stmt)

    } // end of "stmt match"

  /** Find a repeated value in xs, if there is one. */
  private def findRepetition[A](xs: List[A]): Option[A] = 
    if(xs.isEmpty) None
    else if(xs.tail.contains(xs.head)) Some(xs.head)
    else findRepetition(xs.tail)

  /** Typecheck stmts in environment typeEnv. */
  private def typeCheckStmtList(typeEnv: TypeEnv, stmts: List[Statement])
      : Reply[TypeEnv] = {
    // Check bound names are disjoint
    val names = (
      (for(ValueDeclaration(name,_) <- stmts) yield name) ++
      (for(FunctionDeclaration(name,_,_,_,_) <- stmts) yield name)
    )
    findRepetition(names) match{
      case Some(name) => FailureR(s"$name has two definitions") // IMPROVE
      case None =>
        // Extend typeEnv on assumption all FunctionDeclarations are correctly
        // typed.
        val updates = 
          for(FunctionDeclaration(name, tparams, params, rt, body) <- stmts) 
          yield name -> FunctionType(tparams, params.map(_._2), rt)
        iterTypeCheckStmts(typeEnv++updates, stmts)
    }
  }

  /** Typecheck stmts in environment typeEnv.  All names of functions should
    * already be bound to the claimed types. */ 
  private def iterTypeCheckStmts(typeEnv: TypeEnv, stmts: List[Statement])
      : Reply[TypeEnv] =
    if(stmts.isEmpty) Ok(typeEnv)
    else typeCheckStmt(typeEnv, stmts.head).map{ te1 => 
      iterTypeCheckStmts(te1, stmts.tail)
    }

  /** Typecheck a "for" statement. */
  private
  def checkFor(typeEnv: TypeEnv, binders: List[Binder], stmts: List[Statement])
      : Reply[TypeEnv] =
    if(binders.isEmpty) 
      // Typecheck stmts, and end the scope.
      typeCheckStmtList(typeEnv, stmts).map{ case te => Ok(te.endScope) }
    else{
      binders.head match{
        case Generator(name, list) => 
          // list should be a ListType
          typeCheck(typeEnv, list).map{ 
            case (te1, ListType(t)) => 
              checkFor(te1+(name,t), binders.tail, stmts) // bind name
            case (_, t1) => 
              FailureR(s"Expected List, found ${t1.asString}").lift(list)
          }

        case Filter(test) => 
          typeCheckUnify(typeEnv, test, BoolType).map{ case (te, BoolType) =>
            checkFor(te, binders.tail, stmts)
          }
      }
    }


  // ======================================================= Top level

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
