package spreadsheet

// import scala.collection.immutable.{Map,HashMap}

object TypeChecker{
  import TypeVar.TypeID // Type variables (Ints)
  import NameExp.Name // Names of identifiers (Strings)

  /** The next type identifier to use. */
  private var next = 0

  /** Get a new type identifier. */
  private def nextTypeID() : TypeID = { next += 1; next-1 }

  /** Try to unify t1 and t2.  If successful, return updated typeEnv and unified
    * type. */
  def unify(typeEnv: TypeEnv, t1: TypeT, t2: TypeT): Reply[(TypeEnv, TypeT)] = {
    val fail =  FailureR(s"Cannot unify types ${t1.asString} and ${t2.asString}")
    if(t1 == t2) Ok(typeEnv, t1)
    else t1 match{
      case TypeVar(tId1) => 
        val MemberOf(ts1) = typeEnv(tId1)  
        t2 match{
          case TypeVar(tId2) => // Both TypeVars: find intersection         
            val MemberOf(ts2) = typeEnv(tId2); val ts = ts1.intersect(ts2)
            if(ts.isEmpty) fail  
            else if(ts.length == 1){ // map both to that type
              val t = ts.head
              val newTypeEnv = typeEnv.replace(tId1, t).replace(tId2, t)
              Ok(newTypeEnv, t)
            }
            else // replace t2 by t1, with constraint MemberOf(ts)
              Ok(typeEnv.replace(tId2, t1) + (tId1, MemberOf(ts)), t1)
                                                      // TODO: test with ts1!=ts2

          case _ => // t2 a concrete type.  Try to replace t1 by t2
            if(ts1.contains(t2)) Ok(typeEnv.replace(tId1, t2), t2) else fail
        }

      case _ => t2 match{
        case TypeVar(tId2) =>           // Replace t2 by t1
          val MemberOf(ts2) = typeEnv(tId2)
          if(ts2.contains(t1)) Ok(typeEnv.replace(tId2, t1), t1) else fail

        case _ => fail
      }
    }
  }

  // /** Unify the list of types ts. */
  // private def unifyList(typeEnv: TypeEnv, ts: List[TypeT])
  //     : Reply[(TypeEnv, TypeT)] = {
  //   val head = ts.head; val tail = ts.tail
  //   if(tail.isEmpty) Ok((typeEnv, head))
  //   else unifyList(typeEnv, tail).map{ case (te1,tt) => unify(te1, head, tt) }
  // }

  /** Make a FailureR: expected `eType` found `fType` in `exp`. */
  private def mkErr(eType: TypeT, fType: TypeT, exp: Exp) = {
    val source = exp.getExtent.asString
    FailureR(s"Expected ${eType.asString}, found "+fType.asString+
      s"\n\tin $source")
  }

  /** Make a FailureR: expected `eTypes` found `fType` in `exp`. */
  private def mkErr(eTypes: List[TypeT], fType: TypeT, exp: Exp) = {
    assert(exp.getExtent != null, exp)
    val source = exp.getExtent.asString
    val expected = eTypes.map(_.asString).mkString(" or ")
    FailureR(s"Expected $expected, found ${fType.asString}\n\tin $source")
  }

  /** Types that can appear in a cell. */
  val CellTypes = List(IntType, StringType, BoolType)

  /** Contents of the result of a successful call to typeCheck. */
  type TypeCheckRes = (TypeEnv,TypeT)

  /** Typecheck expression `exp` in type environment `typeEnv`. */
  //private 
  def typeCheck(typeEnv: TypeEnv, exp: Exp): Reply[TypeCheckRes] = exp match{
    case NameExp(n) => typeEnv.nameMap.get(n) match{
      case Some(t) => Ok((typeEnv,t))
      case None => FailureR(s"Name not found: $n")
    }

    case IntExp(v) => Ok((typeEnv,IntType))
    case BoolExp(v) => Ok((typeEnv,BoolType))
    case StringExp(st) => Ok((typeEnv,StringType))
    case RowExp(row) => Ok((typeEnv, RowType))
    case ColumnExp(column) => Ok((typeEnv, ColumnType))

    case BinOp(left, op, right) =>
      val tts = binopTypes(op) // expected types
      typeCheck(typeEnv, left).mapOrLift(exp, { case (te1,tl) =>
        val tts1 = tts.filter(_._1 == tl)
        if(tts1.isEmpty) mkErr(tts.map(_._1), tl, left).lift(exp)
        else typeCheck(te1,right).mapOrLift(exp, { case (te2,tr) =>
          val tts2 = tts1.filter(_._2 == tr)
          if(tts2.isEmpty) mkErr(tts1.map(_._2), tr, right).lift(exp)
          else{ assert(tts2.length == 1); Ok(te2,tts2.head._3) }
        })
      })

    case CellExp(column, row) => 
      typeCheck(typeEnv, column).mapOrLift(exp, { case (te1, tc) => 
        if(tc != ColumnType) mkErr(ColumnType, tc, column).lift(exp)
        else typeCheck(te1, row).mapOrLift(exp, { case (te2, tr) => 
          if(tr != RowType) mkErr(RowType, tr, row).lift(exp)
          else{
            val typeId = nextTypeID()
            Ok((typeEnv + (typeId, MemberOf(CellTypes)), TypeVar(typeId)))
          }
        })
      })

    case IfExp(test, thenClause, elseClause) =>
      typeCheck(typeEnv, test).mapOrLift(exp, { case (te1,tt) =>
        unify(te1, tt, BoolType).mapOrLift(exp, { case (te11, bt) => 
          assert(bt == BoolType)
          typeCheck(te11, thenClause).mapOrLift(exp, { case (te2,t1) =>
            typeCheck(te2, elseClause).mapOrLift(exp, { case (te3,t2) =>
              if(t1 == t2) Ok((te3,t1))     // FIXME: or the larger of the two?
              else mkErr(t1, t2, elseClause).lift(exp)
            })
          })
        })
      })

    case ListLiteral(elems) => 
      require(elems.nonEmpty)                                 // IMPROVE
      typeCheckListSingleType(typeEnv, elems).mapOrLift(exp, { case (te1, t) =>
        Ok((te1, ListType(t)))
      })

    case FunctionApp(f, args) => 
      typeCheck(typeEnv, f).mapOrLift(exp, { case (te1, ff) => 
        ff match{
          case FunctionType(domain, range) => 
            // Check actual param types (args) match formal param types (domain)
            if(domain.length != args.length)
              FailureR(s"Expected ${domain.length} arguments, found "+
                args.length).lift(exp)
            else typeCheckListUnify(te1, args, domain).map{ te2 => 
              Ok((te2, range)) 
            }

          case _ => FailureR("Non-function applied as function").lift(exp)
        }
      })

    case BlockExp(stmts, e) => 
      typeCheckStmtList(typeEnv, stmts).map{ te => typeCheck(te, e) }
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
          unify(te2, t1, t2)
        }
    }
  }

  /** Typecheck the list of expressions `exps`.  
    * If successful, return the resulting type environment and list of types.  */
  // private def typeCheckList(typeEnv: TypeEnv, exps: List[Exp])
  //     : Reply[(TypeEnv, List[TypeT])] =
  //   if(exps.isEmpty) Ok((typeEnv, List[TypeT]()))
  //   else typeCheck(typeEnv, exps.head).map{ case (te1, t1) => 
  //     typeCheckList(te1, exps.tail).map{ case (te2, ts) => Ok((te2, t1::ts)) }
  //   }

  /** Type check each element of es, unifying its type with the corresponding
    * element of ts. 
    * Pre: es.length == ts.length, and each element of ts is a concrete type.
    * Used to check parameters of a function application against the expected 
    * types.  */
  private 
  def typeCheckListUnify(typeEnv: TypeEnv, es: List[Exp], ts: List[TypeT])
      : Reply[TypeEnv] = 
    if(es.isEmpty){ assert(ts.isEmpty); Ok(typeEnv) }
    else typeCheck(typeEnv, es.head).map{ case (te1,t1) => 
      unify(te1, t1, ts.head).map{ case (te2, t11) => 
        assert(t11 == ts.head) // ts.head should be concrete
        typeCheckListUnify(te2, es.tail, ts.tail)
      }
    }

  /** Map giving the type of each binary operator. */
  private val binopTypes = Map.from[String, List[(TypeT,TypeT,TypeT)]](
    (for(op <- List("+", "-", "*", "/")) yield
      op -> List((IntType,IntType,IntType)) ) ++
    (for(op <- List("==", "!=")) yield
      op -> List((IntType,IntType,BoolType), (BoolType,BoolType,IntType)) ) ++
    (for(op <- List("<=", "<", ">", ">=")) yield
      op -> List((IntType,IntType,BoolType)) ) ++
    (for(op <- List("||", "&&")) yield
      op -> List((BoolType, BoolType, BoolType)) )
  )

  /** Typecheck the statement `stmt`. 
    * If successful, return the resulting type environment. */
  private 
  def typeCheckStmt(typeEnv: TypeEnv, stmt: Statement): Reply[TypeEnv] = 
    stmt match{
      case Directive(cell, expr) => ???                          // TODO

      case ValueDeclaration(name, exp) => 
        typeCheck(typeEnv, exp).mapOrLift(stmt, { case(te1, t) => 
          Ok(te1 + (name, t))
        })

      case FunctionDeclaration(name, params, rt, body) =>
        // name should already be bound to an appropriate FunctionType
        require(typeEnv(name) == FunctionType(params.map(_._2), rt))
        // Update typeEnv according to params
        val te1 = typeEnv ++ params
        typeCheck(te1, body).mapOrLift(stmt, { case (te2, t) => 
          if(t == rt){                                      // ?? or subtype?
            // Remove from te2 the bindings corresponding to params; replace
            // with the bindings from typeEnv
            var remove = List[Name](); var replace = List[(Name, TypeT)]()
            for((x,_) <- params) typeEnv.nameMap.get(x) match{
              case Some(ts) => replace ::= ((x,ts))
              case None => remove ::= x
            }
            // IMPROVE: I think we can just use typeEnv.nameMap
            Ok(TypeEnv(te2.nameMap -- remove ++ replace, te2.constraints))
          }
          else mkErr(rt, t, body).lift(stmt) // wrong return type
        })

    } // end of "stmt match"

  /** Typecheck stmts in environment typeEnv. */
  def typeCheckStmtList(typeEnv: TypeEnv, stmts: List[Statement])
      : Reply[TypeEnv] = {
    // Extend typeEnv on assumption all FunctionDeclarations are correctly
    // typed.
    val updates = (
      for(fd @ FunctionDeclaration(name, params, rt, body) <- stmts) yield 
        name -> FunctionType(params.map(_._2), rt)
    )
    val typeEnv1 = TypeEnv(typeEnv.nameMap ++ updates, typeEnv.constraints)
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


}
