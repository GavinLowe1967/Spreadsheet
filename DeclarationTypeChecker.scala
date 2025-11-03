package spreadsheet

import TypeParam.TypeParamName // Names of type parameters (Strings)

/** Type checker for declarations. */
object DeclarationTypeChecker extends DeclarationTypeCheckerT{
  /** The ExpTypeChecker object, for type checking expressions. */
  val etc = new ExpTypeChecker(this)

  private val typeCheckAndClose = etc.typeCheckAndClose _
  private val typeCheckUnifyAndClose = etc.typeCheckUnifyAndClose _ 

  /** Find a repeated value in xs, if there is one. */
  private def findRepetition[A](xs: List[A]): Option[A] = 
    if(xs.isEmpty) None
    else if(xs.tail.contains(xs.head)) Some(xs.head)
    else findRepetition(xs.tail)

  /** Type check the Declaration decl, returning the resulting type envionment
    * if successful. */
  def typeCheckDecl(typeEnv: TypeEnv, decl: Declaration): Reply[TypeEnv] = 
    decl match{
      case ValueDeclaration(name, exp) => 
        typeCheckAndClose(typeEnv, exp).mapOrLift(decl, { case (te1, t) => 
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
                typeCheckUnifyAndClose(te1, body, rt).map{ case (te3, tt) =>
                  Ok(te3.endScope) // back to the old scope
                }
// FIXME: if any of tparams gets bound to a TypeVar, update in typeEnv(name)
          }
        }).lift(decl)
    }

  /** Check the names bound in stmts are disjoint.  If so, return the type
    * environment formed by binding the function names to their claimed types,
    * and removing val names that duplicate names in an outer block. */
  def checkDisjointNames(typeEnv: TypeEnv, stmts: List[Statement])
      : Reply[TypeEnv] = {
    // Check bound names are disjoint
    val valNames = for(ValueDeclaration(name,_) <- stmts) yield name 
    val fnDecs = Statement.getFnDecs(stmts)
    val names = valNames ++ fnDecs.map(_.name)
    findRepetition(names) match{
      case Some(name) => FailureR(s"$name has two definitions") // IMPROVE
      case None =>
        // Extend typeEnv on assumption all FunctionDeclarations are correctly
        // typed, but remove overwritten names.
        val updates = 
          for(FunctionDeclaration(name, tparams, params, rt, body) <- fnDecs) 
          yield name -> FunctionType(tparams, params.map(_._2), rt)
        Ok(typeEnv -- valNames ++ updates)
    }
  }

  /** Type check decls. */
  def typeCheckDeclList(typeEnv: TypeEnv, decls: List[Declaration])
      : Reply[TypeEnv] = 
    // Check names are disjoint
    checkDisjointNames(typeEnv, decls).map{ te1 =>  // Iterate along decls
      Reply.fold(typeCheckDecl, te1, decls)
    }
}
