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

  /** Find a pair of elements in xs that satisfy f. */
  private def findPair[A](f: (A,A) => Boolean, xs: Array[A]): Option[(A,A)] = {
    val len = xs.length; var i = 0; var result = None: Option[(A,A)]
    while(i < len-1 && result == None){
      var j = i+1; val x = xs(i)
      while(j < len && !f(x, xs(j))) j += 1
      if(j < len) result = Some((x, xs(j))) else i += 1
    }
    result
  }


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
        val Some(ts) = typeEnv.get(name)
        require(ts.contains(FunctionType(tparams, params.map(_._2), rt)))
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

  /** Check that the definitions `defs` for function `name` is allowed: if there
    * is more than one definition, that none is polymorphic, and that they
    * have disjoint types.  If not, return appropriate FailureR; return null
    * if ok. */
  private 
  def checkOverloadingAllowed(name: String, defs: List[FunctionDeclaration])
      : Reply[TypeEnv] = {
    val len = defs.length
    if(len > 1){
      val poly = defs.filter(_.tParams.nonEmpty)
      if(poly.nonEmpty)
        FailureR(
          s"Function $name with multiple definitions has polymorphic\n"+
            s"instance at line ${poly.head.lineNumber}.") 
      else{
        // Search for definitions with same argument types.
        var i = 0; var result: Reply[TypeEnv] = null
        while(i < len-1 && result == null){
          val paramTs: List[TypeT] = defs(i).paramTs; var j = i+1
          while(j < len && result == null){
            if(defs(j).paramTs == paramTs)
              result = FailureR(
                s"Function $name has multiple definitions with\n"+
                  "parameters of type "+TypeT.showList(paramTs)
              ).addLines(defs(i),defs(j))
            j += 1
          }
          i += 1
        } // end of outer while
        result
        // FIXME: check disjoint types
      }
    }
    else null
  }

  /** Function declarations grouped together by name. */
  private type GroupedFunctions = Map[String, List[FunctionDeclaration]] 

  /** Check that any overloading of names is done correctly.  If so, return the
    * type environment formed by binding the function names to their claimed
    * types, and removing val names that duplicate names in an outer block.
    * Also label overloaded function declarations with their index.  */
  def checkOverloading(typeEnv: TypeEnv, stmts: List[Statement])
      : Reply[TypeEnv] = {
    def sameName(vd1: ValueDeclaration, vd2: ValueDeclaration) = 
      vd1.name == vd2.name
    val valDecs = for(vd @ ValueDeclaration(_,_) <- stmts) yield vd
    val fnDecs = for(fd @ FunctionDeclaration(_,_,_,_,_) <- stmts) yield fd
    val grouped: GroupedFunctions = fnDecs.groupBy(_.name)
    val fnNames = grouped.keys
    // Names in both valDecs and fnDecs 
    val repeats = valDecs.filter(vd => grouped.contains(vd.name))
    if(repeats.nonEmpty){
      val vd = repeats.head; val name = vd.name
      FailureR(s"$name has both val and def definitions at lines "+
        (vd::grouped(name)).map(_.lineNumber).mkString(", ")+".")
    }
    else findPair(sameName, valDecs.toArray) match{
      case Some((vd1,vd2)) => 
        FailureR(s"${vd1.name} has two val definitions at lines "+
          s"${vd1.lineNumber} and ${vd2.lineNumber}.")
      case None =>
        // Iterate over grouped, and check each
        var result: Reply[TypeEnv] = null; val iter = grouped.iterator
        while(iter.hasNext && result == null){
          val (name,defs) = iter.next()
          result = checkOverloadingAllowed(name, defs)
        }
        if(result != null) result
        else updateEnv(typeEnv -- valDecs.map(_.name), grouped)
    }
  }

  /** Extend typeEnv with the claimed types of function declarations.  Also
    * label each function declaration with its index. */
  private def updateEnv(typeEnv: TypeEnv, grouped: GroupedFunctions)
      : Reply[TypeEnv] = {
    var te = typeEnv 
    for((name,defs) <- grouped){
      // Label each element of defs with its index in defs (unless
      // defs is a singleton); and build list of types.
      var ts = List[TypeT](); var i = 0
      for(fd <- defs){
        if(defs.length > 1){ fd.setIndex(i); i += 1 }
        ts = ts :+ fd.mkFunctionType
      }
      te = te + (name, ts)
    }
    Ok(te)
  }

  /** Type check decls. */
  def typeCheckDeclList(typeEnv: TypeEnv, decls: List[Declaration])
      : Reply[TypeEnv] = 
    checkOverloading(typeEnv, decls).map{ te1 =>  // Iterate along decls
      Reply.fold(typeCheckDecl, te1, decls)
    }
}
