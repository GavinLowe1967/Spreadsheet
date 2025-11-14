package spreadsheet

/** The type checker for statements. */
object TypeChecker{
  /** The ExpTypeChecker object. */
  private val etc = DeclarationTypeChecker.etc
  private val typeCheckAndClose = etc.typeCheckAndClose _
  private val typeCheckUnifyAndClose = etc.typeCheckUnifyAndClose _ 

  import DeclarationTypeChecker.{typeCheckDecl,checkOverloading}

  /** Typecheck the statement `stmt`. 
    * If successful, return the resulting type environment. */
  private 
  def typeCheckStmt(typeEnv: TypeEnv, stmt: Statement): Reply[TypeEnv] = 
    stmt match{
      case decl: Declaration => typeCheckDecl(typeEnv, decl)

      case Directive(column, row, expr) =>
        typeCheckAndClose(typeEnv, expr).map{ case (te1, t) => 
          if(! TypeT.CellTypes.contains(t))
            FailureR(s"Expected cell type, found ${t.asString}").lift(expr,true)
          else
            typeCheckUnifyAndClose(te1, row, RowType).map{ case(te2, RowType) =>
              typeCheckUnifyAndClose(te2, column, ColumnType).map{
                case(te3, ColumnType) => Ok(te3)
              }
            }
        }.lift(stmt)

      case ForStatement(qs, stmts) => 
        etc.checkQualifiers(typeEnv.newScope, qs).map{ te1 =>
          typeCheckStmtList(te1, stmts).map{ te2 => Ok(te2.endScope) }
        }.lift(stmt)
        // checkFor(typeEnv.newScope, binders, stmts).lift(stmt)

    } // end of "stmt match", typeCheckStmt

  /** Typecheck stmts in environment typeEnv. */
  private def typeCheckStmtList(typeEnv: TypeEnv, stmts: List[Statement])
      : Reply[TypeEnv] =
    checkOverloading(typeEnv, stmts).map{ te1 => // Iterate along stmts
      Reply.fold(typeCheckStmt, te1, stmts)
    }

  // /** Typecheck a "for" statement. */
  // private
  // def checkFor(typeEnv: TypeEnv, qs: List[Qualifier], stmts: List[Statement])
  //     : Reply[TypeEnv] =
  //   etc.checkQualifiers(typeEnv, qs).map{ te1 => 
  //     typeCheckStmtList(te1, stmts).map{ te2 => Ok(te2.endScope) }
  //   }
/*
    if(binders.isEmpty) 
      // Typecheck stmts, and end the scope.
      typeCheckStmtList(typeEnv, stmts).map{ case te => Ok(te.endScope) }
    else{
      binders.head match{
        case Generator(name, list) => 
          // list should be a ListType
          typeCheckAndClose(typeEnv, list).map{ 
            case (te1, ListType(t)) => 
              checkFor(te1+(name,t), binders.tail, stmts) // bind name
            case (_, t1) => 
              FailureR(s"Expected List, found ${t1.asString}").lift(list)
          }
        case Filter(test) => 
          typeCheckUnifyAndClose(typeEnv, test, BoolType).map{ 
            case (te, BoolType) => checkFor(te, binders.tail, stmts)
          }
      }
    }
 */

  // /** Typecheck qs, returning an updated TypeEnv if successful. */
  // def checkQualifiers(typeEnv: TypeEnv, qs: List[Qualifier]): Reply[TypeEnv] = 
  //   Reply.fold(checkQualifier, typeEnv, qs)

  // /** Typecheck q, returning an updated TypeEnv if successful. */
  // private def checkQualifier(typeEnv: TypeEnv, q: Qualifier): Reply[TypeEnv] =
  //   q match{
  //     case Generator(name, list) =>
  //       // list should be a ListType
  //       typeCheckAndClose(typeEnv, list).map{
  //         case (te1, ListType(t)) => Ok(te1+(name,t)) // bind name
  //         case (_, t1) =>
  //           FailureR(s"Expected List, found ${t1.asString}").lift(list)
  //       }
  //     case Filter(test) =>
  //       typeCheckUnifyAndClose(typeEnv, test, BoolType).map{
  //         case (te, BoolType) => Ok(te)
  //       }
  //   }

  // ======================================================= Top level

  /** Typecheck stmts. */
  def apply(stmts: List[Statement]): Reply[TypeEnv] =
    typeCheckStmtList(TypeEnv(), stmts)

  // ========= Testing

  val outer = this

  /** Test hooks, to give TypeCheckerTest access to private operations. */
  object TestHooks{
    val typeCheckStmtList = outer.typeCheckStmtList _
  }

}
