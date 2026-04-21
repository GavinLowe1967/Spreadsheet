package spreadsheet

import TypeParam.TypeParamName // Names of type parameters (Strings)

/** The type checker for statements. */
object TypeChecker extends TypeCheckerT{
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

  /** Type check fd, returning the resulting type envionment if successful. */
  private def typeCheckDecl(typeEnv: TypeEnv, fd: FunctionDeclaration)
      : Reply[TypeEnv] = {
    val FunctionDeclaration(name, tparams, paramss, ort, body) = fd
    val params = paramss.flatten
    // If return type is given, name should already be bound to an
    // appropriate FunctionType, by typeCheckStmtList
    if(ort.isDefined){
      val Some(ts) = typeEnv.get(name); require(ts.contains(fd.mkFunctionType))
    }
    // Check names of params, tparams are disjoint
    findRepetition(params.map(_._1)) match{
      case Some(p) => FailureR(s"Repeated parameter $p")
      case None => findRepetition(tparams.map(_._1)) match{
        case Some(tp) => FailureR(s"Repeated type parameter $tp")
        case None =>
          // Create a new scope, and extend with params and tparams
          val te1 = (typeEnv.newScope ++ params).addTypeParams(tparams)
          // Type parameters used in params
          val usedTParams: List[TypeParamName] =
            params.flatMap(_._2.typeParams) ++
              (if(ort.isDefined) ort.get.typeParams else List())
          val invalidTParams = usedTParams.filter(p => !te1.hasTypeParam(p))
          if(invalidTParams.nonEmpty)
            FailureR(s"Unknown type(s): "+invalidTParams.mkString(", "))
          else ort match{
            case Some(rt) =>
              // Typecheck body, and make sure return type matches rt
              typeCheckUnifyAndClose(te1, body, rt).map{ case (te2, tt) =>
                Ok(te2.endScope) // back to the old scope
              }
            case None =>
              // Typecheck body.  Add appropriate FunctionType value.
              typeCheckAndClose(te1, body).map{ case (te2, rt) =>
                val ft = fd.mkFunctionType(rt); Ok(te2.endScope.update(name, ft))
              }
          } // end of match
// FIXME: if any of tparams gets bound to a TypeVar, update in typeEnv(name)
      }
    }
  }

  //========= Handling of overloaded functions

  /** Check that the definitions `defs` for function `name` is allowed: if there
    * is more than one definition, then they have disjoint types: we require
    * them to differ in the first list of parameters.  If not, return
    * appropriate FailureR; return null if ok. */
  private 
  def checkOverloadingAllowed(name: String, defs: List[FunctionDeclaration])
      : Reply[TypeEnv] = {
    val len = defs.length
    if(len > 1){
      // Search for definitions with same first argument types.
      var i = 0; var result: Reply[TypeEnv] = null
      while(i < len-1 && result == null){
        val paramTs = defs(i).paramTs.head; var j = i+1
        while(j < len && result == null){
          if(defs(j).paramTs.head == paramTs)
            result = FailureR(
              s"Function $name has multiple definitions with\n"+
                "parameters of type "+TypeT.showList(paramTs)
            ).addLines(defs(i),defs(j))
          j += 1
        }
        i += 1
      } // end of outer while
      result
    }
    else null
  }

  /** Function declarations grouped together by name. */
  private type GroupedFunctions = Map[String, List[FunctionDeclaration]] 

  /** Check that any overloading of names is done correctly.  If so, return the
    * type environment formed by binding the function names to their claimed
    * types, and removing val-names that duplicate names in an outer block.
    * Also label overloaded function declarations with their index.  */
  def checkOverloading(typeEnv: TypeEnv, stmts: List[Statement])
      : Reply[TypeEnv] = {
    val valDecs = (for(vd @ ValueDeclaration(_,_) <- stmts) yield vd).toArray
    val fnDecs = for(fd @ FunctionDeclaration(_,_,_,_,_) <- stmts) yield fd
    val grouped: GroupedFunctions = fnDecs.groupBy(_.name)
    val fnNames = grouped.keys
    // Search for repeated name declarations.
    for(i <- 0 until valDecs.length){
      val vd = valDecs(i); val names = vd.pattern.names
      // Is a name bound twice in vd?
      val doubleBinds = findPair[String]((_ == _), names.toArray)
      if(doubleBinds.nonEmpty){
        val (n1,n2) = doubleBinds.get; assert(n1 == n2)
        return FailureR(s"$n1 bound twice in declaration").lift(vd,true)
      }
      // Is a name declared in vd and a def?
      val repeats = names.filter(n => grouped.contains(n))
      if(repeats.nonEmpty){
        val name = repeats.head
        return FailureR(s"$name has both val and def definitions at lines "+
          (vd::grouped(name)).map(_.lineNumber).mkString(", ")+".")
      }
      // Is a name bound in vd and another val-dec?
      for(j <- i+1 until valDecs.length){
        val vd1 = valDecs(j); val common = names.intersect(vd1.boundNames)
        if(common.nonEmpty){
          val name = common.head
          return FailureR(s"${name} has two val definitions at lines "+
            s"${vd.lineNumber} and ${vd1.lineNumber}.")
        }
      }
    } // end of for loop
    // Iterate over grouped, and check each
    var result: Reply[TypeEnv] = null; val iter = grouped.iterator
    while(iter.hasNext && result == null){
      val (name,defs) = iter.next(); result = checkOverloadingAllowed(name, defs)
    }
    if(result != null) result
    else{
      val te1 = typeEnv -- valDecs.toList.flatMap(_.pattern.names)
      updateEnv(te1, grouped)
    }
  }


/*
    val repeats = 
      valDecs.filter(vd => vd.pattern.names.exists(n => grouped.contains(n)))
    if(repeats.nonEmpty){
      val vd = repeats.head; val name = vd.pattern.names.head // FIXME
      FailureR(s"$name has both val and def definitions at lines "+
        (vd::grouped(name)).map(_.lineNumber).mkString(", ")+".")
    }
    else 
 */

  /** Extend typeEnv with the claimed types of function declarations where a
    * return type is given.  Also label each function declaration with its
    * index. */
  private def updateEnv(typeEnv: TypeEnv, grouped: GroupedFunctions)
      : Reply[TypeEnv] = {
    var te = typeEnv 
    for((name,defs) <- grouped){
      // Label each element of defs with its index in defs (unless
      // defs is a singleton); and build list of types.
      var ts = List[TypeT](); var i = 0
      for(fd <- defs){
        if(defs.length > 1){ fd.setIndex(i); i += 1 }
        /*if(fd.ort.isDefined)*/ ts = ts :+ fd.mkFunctionType
      }
      te = te + (name, ts)
    }
    Ok(te)
  }

  // ========= Top-level functions


  /** Typecheck the statement `stmt`. 
    * If successful, return the resulting type environment. */
  private 
  def typeCheckStmt(typeEnv: TypeEnv, stmt: Statement): Reply[TypeEnv] = 
    stmt match{
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

      case ValueDeclaration(pat, exp) => 
        typeCheckAndClose(typeEnv, exp).map{ case (te1, t) => 
          etc.bindNames(te1, pat, t)
        }.lift(stmt)

      case fd: FunctionDeclaration =>  typeCheckDecl(typeEnv, fd).lift(fd)

      case Assertion(condition) => 
        typeCheckUnifyAndClose(typeEnv, condition, BoolType).
          mapOrLift(stmt, { case (te, _) => Ok(te) })

      case Assertion2(condition, msg) => 
        typeCheckUnifyAndClose(typeEnv, condition, BoolType).map{ case (te, _) =>
          typeCheckUnifyAndClose(te, msg, StringType).map{ case (te1,_) => 
            Ok(te1) }
        }.lift(stmt)

      case CallStatement(e) =>
        typeCheckUnifyAndClose(typeEnv, e, UnitType).map{ 
          case (te1, `UnitType`) => Ok(te1)
        }.lift(stmt)

      case IfStatement(condition, ifCase, elseCase) =>
        typeCheckUnifyAndClose(typeEnv, condition, BoolType).map{ case (te1,_) =>
          typeCheckStmtList(te1, ifCase).map{ case te2 => 
            typeCheckStmtList(te2, elseCase)
          }
        }.lift(stmt)
    } // end of "stmt match", typeCheckStmt

  /** Typecheck stmts in environment typeEnv. */
  def typeCheckStmtList(typeEnv: TypeEnv, stmts: List[Statement])
      : Reply[TypeEnv] =
    checkOverloading(typeEnv, stmts).map{ te1 => // Iterate along stmts
      Reply.fold(typeCheckStmt, te1, stmts)
    }

  // ======================================================= Top level

  /** Typecheck stmts. */
  def apply(stmts: List[Statement]): Reply[TypeEnv] =
    typeCheckStmtList(TypeEnv(), stmts)
}
