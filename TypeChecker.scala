package spreadsheet

import scala.collection.immutable.{Map,HashMap}

object TypeChecker{

  import TypeVar.TypeID // Type variables (Ints)
  import NameExp.Name // Names of identifiers (Strings)

  /** Information stored about the type of a name. */
  case class TypeScheme(t: TypeT){ }

  type TypeEnv = HashMap[Name, TypeScheme]

  //private var next = 0

  /** Get a new type identifier. */
  //private def nextTypeID() : TypeID = { next += 1; next-1 }

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

  type TypeCheckRes = (TypeEnv,TypeT)

  /** Typecheck expression `exp` in type environment `typeEnv`. */
  //private 
  def typeCheck(typeEnv: TypeEnv, exp: Exp): Reply[TypeCheckRes] = exp match{
    case NameExp(n) => typeEnv.get(n) match{
      case Some(TypeScheme(t)) => Ok((typeEnv,t))
      case None => FailureR(s"Name not found: $n")
    }

    case IntExp(v) => Ok((typeEnv,IntType))
    case BoolExp(v) => Ok((typeEnv,BoolType))
    case StringExp(st) => Ok((typeEnv,StringType))
    case RowExp(row) => Ok((typeEnv, RowType))
    case ColumnExp(column) => Ok((typeEnv, ColumnType))

    case BinOp(left, op, right) =>
      val tts = binopTypes(op) // expected types
      typeCheck(typeEnv, left).mapOrLift(exp, { case(te1,tl) =>
        val tts1 = tts.filter(_._1 == tl)
        if(tts1.isEmpty) mkErr(tts.map(_._1), tl, left).lift(exp)
        else typeCheck(te1,right).mapOrLift(exp, { case(te2,tr) =>
          val tts2 = tts1.filter(_._2 == tr)
          if(tts2.isEmpty) mkErr(tts1.map(_._2), tr, right).lift(exp)
          else{ assert(tts2.length == 1); Ok(te2,tts2.head._3) }
        })
      })

    case CellExp(column, row) => ???                            // TODO

    case IfExp(test, thenClause, elseClause) =>
      typeCheck(typeEnv, test).mapOrLift(exp, { case (te1,tt) =>
        if(tt != BoolType) mkErr(BoolType, tt, test).lift(exp)
        else typeCheck(te1, thenClause).mapOrLift(exp, { case (te2,t1) =>
          typeCheck(te2, elseClause).mapOrLift(exp, { case (te3,t2) =>
            if(t1 == t2) Ok((te3,t1))     // FIXME: or the larger of the two?
            else mkErr(t1, t2, elseClause).lift(exp)
          })
        })
      })

    case ListLiteral(elems) => 
      require(elems.nonEmpty)                                 // IMPROVE
      typeCheckList(typeEnv, elems).mapOrLift(exp, { case(te1, ts) => 
        val t1 = ts.head
        // Check all other elements have type t1
        val errs = elems.zip(ts).filter(_._2 != t1)
        if(errs.isEmpty) Ok((te1, ListType(t1)))
        else{ val (e2,t2) = errs.head; mkErr(t1, t2, e2).lift(exp) }
      })

    case FunctionApp(f, args) => 
      typeCheck(typeEnv, f).mapOrLift(exp, { case (te1, ff) => 
        ff match{
          case FunctionType(domain, range) => 
            // Check actual param types (argsT) match formal param types (domain)
            if(domain.length != args.length)
              FailureR(s"Expected ${domain.length} arguments, found "+
                args.length).lift(exp)
            else typeCheckList(te1, args).mapOrLift(exp, { case (te2, argTs) => 
              if(domain == argTs) Ok((te2, range))
              else{ 
                // Find first place where different
                val errs = args.zip(argTs).zip(domain).filter{ 
                  case ((v,t),ft) => t != ft
                }
                val ((v,t),ft) = errs.head; mkErr(ft, t, v).lift(exp)
              }
            })

          case _ => FailureR("Non-function applied as function").lift(exp)
        }
      })

    case BlockExp(stmts, e) => 
      typeCheckStmtList(typeEnv, stmts).map{ te => typeCheck(te, e) }
  }

  /** Iterate `f` along `xs` as far as successful, passing through the updated
    * `TypeEnv`, and building a list of the results. */
  // private def iterate[B, C](
  //   f: (TypeEnv,B) => Reply[(TypeEnv,C)], xs: List[B], te: TypeEnv)
  //     : Reply[(TypeEnv, List[C])] =
  //   if(xs.isEmpty) Ok((te, List()))
  //   else f(te,xs.head).map{ case(te1, c) => 
  //     iterate(f, xs.tail, te1).map{ case(te2, cs) => Ok((te2, c::cs)) }
  //   }

  /** Typecheck the list of expressions `exps`.  
    * If successful, return the resulting type environment and type. */
  private def typeCheckList(typeEnv: TypeEnv, exps: List[Exp])
      : Reply[(TypeEnv, List[TypeT])] =
    // iterate(typeCheck, exps, typeEnv)
    if(exps.isEmpty) Ok((typeEnv, List[TypeT]()))
    else typeCheck(typeEnv, exps.head).map{ case(te1, t1) => 
      typeCheckList(te1, exps.tail).map{ case(te2, ts) => Ok((te2, t1::ts)) }
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
          Ok(te1 + (name -> TypeScheme(t)))
        })

      case FunctionDeclaration(name, params, rt, body) =>
        // name should already be bound to an appropriate FunctionType
        require(typeEnv(name) == TypeScheme(FunctionType(params.map(_._2), rt)))
        // Update typeEnv according to params
        val te1 = typeEnv ++ params.map{ case (x,t) => (x -> TypeScheme(t)) }
        typeCheck(te1, body).mapOrLift(stmt, { case (te2, t) => 
          if(t == rt){                                      // ?? or subtype?
            // Remove from te2 the bindings corresponding to params; replace
            // with the bindings from typeEnv
            var remove = List[Name](); var replace = List[(Name, TypeScheme)]()
            for((x,_) <- params) typeEnv.get(x) match{
              case Some(ts) => replace ::= ((x,ts))
              case None => remove ::= x
            }
            Ok(te2 -- remove ++ replace)
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
        (name -> TypeScheme(FunctionType(params.map(_._2), rt)))
    )
    typeCheckStmtList1(typeEnv ++ updates, stmts) 
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
