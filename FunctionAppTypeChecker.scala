package spreadsheet

import Unification.unify
import TypeVar.{TypeID,nextTypeID} // Type variables (Ints)
import FunctionType.TypeParameter  // (TypeParamName, TypeParamConstraint)
import TypeParam.TypeParamName   // Names of type parameters (Strings)
import NameExp.Name // Names of identifiers (Strings)
import TypeT.showList
import TypeChecker0.{TypeCheckRes}
import Substitution.{
  replaceTypeParamsByTypeVars, TypeMap, emptyTypeMap, ReverseTypeMap, inverse, 
  union, emptyRevMap, reverseRemapBy, subTypeParamsInResult}

/** Type checking of function applications. */
class FunctionAppTypeChecker(etc: ExpTypeCheckerT){

  private var nextNameIx = 0

  /** Get a new Name. */
  private def newName(): Name = { nextNameIx += 1; "%"+nextNameIx } 

  /** Typecheck the application of a value of type t (not necessarily a
    * FunctionType) to args. */
  def checkFunctionApp(typeEnv: TypeEnv, t: TypeT, args: List[Exp])
      : TypeCheckRes =
    t match{
      case ft: FunctionType => checkFunctionApp1(typeEnv, ft, args)
      case _ => FailureR("Non-function applied as function")
    }

  private val forwardRefFail = FailureR(
    s"Function without explicit return type applied before its definition")

  /** Typecheck the application of a function of type ft to args. */
  private 
  def checkFunctionApp1(typeEnv: TypeEnv, ft: FunctionType, args: List[Exp])
      : TypeCheckRes = {
// println(s"checkFunctionApp1(\n\t$ft,\n\t$args)")
    val FunctionType(tParams, domain, range) = ft
    if(domain.length != args.length)
      FailureR(s"Expected ${domain.length} arguments, found "+args.length)
    else if(range == null) forwardRefFail
    else{
      //val unusedTParams = ft.unusedTParams
      // Create fresh type variables to replace tParams in domain and range
      val (te1, domain1, range1, typeMap) =
        Substitution.replaceTypeParamsByTypeVars(
          typeEnv.newScope, ft.usedTParams, domain, range)
      checkFunctionApp2(te1, domain1, range1, ft.typeParams.toSet, args).map{
        case (te2, res0) => 
          val (te3, res) = subTypeParamsInResult(te2, ft.unusedTParams, res0)
          Ok((te3.endScope, res))
      }
    }
  }

      // // Generate a new name, and bind it to range1 in the environment;
      // // then unify the types of args with domain1, so the new name gets
      // // updated to the appropriate return type.
      // val name = newName(); val te2 = te1 + (name, range1)
      // typeCheckList(te2, ft.typeParams.toSet, args).map{ case (te3, argTs) =>
      //   unifyList(te3, argTs, domain1).map{ case (te4, invMap) =>
      //     // Extract type of name.  Need to apply invMap to reverse renaming 
      //     // done in unifyList.
      //     val res0 = reverseRemapBy(invMap, te4(name))
      //     val (te5, res) = subTypeParamsInResult(te4, unusedTParams, res0)
      //     Ok((te5.endScope, res))

  /** Type check function of type domain => range to arguments args.  If
    * successful, return the resulting environment and the type of the
    * application.  `typeParams` is the type parameters bound in the type of
    * the function.  */
  private def checkFunctionApp2(
    typeEnv: TypeEnv, domain: List[TypeT], range: TypeT, 
    typeParams: Set[TypeParamName], args: List[Exp])
      : TypeCheckRes = {
    // Generate a new name, and bind it to range in the environment;
    // then unify the types of args with domain, so the new name gets
    // updated to the appropriate return type.
    val name = newName(); val te1 = typeEnv + (name, range)
    typeCheckList(te1, typeParams, args).map{ case (te2, argTs) =>
      unifyList(te2, argTs, domain).map{ case (te3, invMap) =>
        // Extract type of name.  Need to apply invMap to reverse renaming
        // done in unifyList.
        Ok((te3, reverseRemapBy(invMap, te3(name))))
      }
    }
  }

/*
      typeCheckListUnify(te2, args, domain1, ft.typeParams.toSet).map{ 
        case (te3, invMap) =>
          // Extract type of name.  Need to apply invMap to reverse renaming 
          // done in typeCheckListUnify.
          val tParams = invMap.toList.map(_._2) // type parameters
          val res0 = reverseRemapBy(invMap, tParams, te3(name))
          // Add unusedTParams to FunctionType results, and replace other type
          // parameters by fresh type variables.
          val (te4, res) =
            Substitution.subTypeParamsInResult(te3, unusedTParams, res0)
          Ok((te4.endScope, res))
      }
 */

  /** Type check each element of args.  Rename bound type parameters to avoid
    * clashes with elements of fnTParams.  If successful, return resulting
    * environment, and list of types. */
  private def typeCheckList(
    typeEnv: TypeEnv, fnTParams: Set[TypeParamName], args: List[Exp])
      : Reply[(TypeEnv, List[TypeT])] = {
    if(args.isEmpty) Ok((typeEnv, List[TypeT]()))
    else etc.typeCheck(typeEnv, args.head).map{ case (te1, t1) =>
      // Rename type parameters to avoid name clashes.
      val t2 = t1.renameTypeParams(TypeParam.newTypeParamMap, fnTParams)
      typeCheckList(te1, fnTParams, args.tail).map{ case (te2, ts) =>
        Ok(te2, t2::ts)
      }
    }
  }

  /** Unify each actual type in argTs against the type of the corresponding
    * formal parameter in paramTs.  If successful, return resulting
    * environment and map to map type variables back to the corresponding type
    * parameters.  */
  private 
  def unifyList(typeEnv: TypeEnv, argTs: List[TypeT], paramTs: List[TypeT])
      : Reply[(TypeEnv, ReverseTypeMap)] = 
    if(argTs.isEmpty){ assert(paramTs.isEmpty); Ok((typeEnv, emptyRevMap)) }
    else{
      // Replace type parameters in argTs.head by type variables
      val (te1, t1, typeMap) = mkInstance(typeEnv, argTs.head)
      unify(te1, t1, paramTs.head).map{ case (te2, _) =>
        unifyList(te2, argTs.tail, paramTs.tail).map{ case (te3, invMap) =>
          Ok((te3, union(invMap, inverse(typeMap))))
        }
      }
    }
  
/*
  /** Type check each element of es, unifying its type with the corresponding
    * element of ts.  Avoid name clashes with names in fnTParams.  If
    * successful, return resulting environment and map to map type variables
    * back to type parameters.
    * Pre: es.length == ts.length.
    * Used to check actual parameters es of a function application against the
    * expected types ts.  */
  private 
  def typeCheckListUnify(typeEnv: TypeEnv, es: List[Exp], ts: List[TypeT], 
    fnTParams: Set[TypeParamName])
      : Reply[(TypeEnv, ReverseTypeMap)] = 
    if(es.isEmpty){ assert(ts.isEmpty); Ok((typeEnv, emptyRevMap)) }
    else etc.typeCheck(typeEnv, es.head).map{ case (te1,t1) => 
      // Rename type parameters to avoid name clashes.
      val t11 = t1.renameTypeParams(TypeParam.newTypeParamMap, fnTParams)
//if(t11 != t1) println(s"**typeCheckListUnify:\n${es.head}: $t1 (${ts.head}) ->\n  $t11")
      val (te2, t2, typeMap) = mkInstance(te1,t11) 
//println(s"typeCheckListUnify:\n\t${es.head}: $t1\n\t -> $t2;\n\t unifying with ${ts.head} \n via $typeMap")
      // Unify with formal parameter type in ts, and recurse.
      unify(te2, t2, ts.head).lift(es.head, true).map{ case (te3, _) => 
        typeCheckListUnify(te3, es.tail, ts.tail, fnTParams).map{ 
          case (te5, invMap) => Ok((te5, union(invMap, inverse(typeMap))))
        }
      }
    }
 */

  /** If t is a FunctionType, make an instance of it, replacing each type
    * parameter by a fresh type variable, and add suitable constraints to
    * typeEnv. */
  private def mkInstance(typeEnv: TypeEnv, t: TypeT)
      : (TypeEnv, TypeT, TypeMap) = t match{
    case FunctionType(tParams, domain, range) => 
      val (te, domain1, range1, typeMap) = 
        replaceTypeParamsByTypeVars(typeEnv, tParams, domain, range)
      (te, FunctionType(List(), domain1, range1), typeMap)
    case _ => (typeEnv, t, emptyTypeMap)
// FIXME, what about a type like ListType(FunctionType(_,_,_)) ???
  }

/*
  private def addTParams(tParams: List[TypeParameter], t: TypeT): TypeT =
    t match{
      case FunctionType(tp, domain, range) => 
        for((t1,_) <- tp; (t2,_) <- tParams) assert(t1 != t2)
        FunctionType(tp++tParams, domain, range)
      case ListType(u) => ListType(addTParams(tParams, u))
      case TypeParam(tp) => 
        for((tp1,_) <- tParams) 
          assert(tp1 != tp, s"tp = $tp; tParams = $tParams")
        t
      case _ => t
    }
 */

  /** Type check es in turn.  If successful, return the resulting type
    * environment and list of types. */
  private def typeCheckList(typeEnv: TypeEnv, es: List[Exp])
      : Reply[(TypeEnv, List[TypeT])] = {
    if(es.isEmpty) Ok(typeEnv, List[TypeT]())
    else etc.typeCheck(typeEnv, es.head).map{ case(te1, t1) => 
      val (te2,t2,typeMap) = mkInstance(te1,t1)
      typeCheckList(te2, es.tail).map{ case (te3,ts) => Ok(te3, t2::ts) }
    }
  }

  /** Typecheck the arguments of fa, and find the type from ts for the function.
    * If successful, store the index in fa.  */
  def findFunctionApp(typeEnv: TypeEnv, fa: FunctionApp, ts: Array[FunctionType])
      : Reply[(TypeEnv, TypeT)] = {
    val FunctionApp(ne @ NameExp(fn), args) = fa
    assert(ts.length >= 2 && ts.forall(_.params.isEmpty))
    // Get types of actual parameters
    typeCheckList(typeEnv.newScope, args).lift(fa).map{ case (te1, argsTs) => 
      // Find those elements that match
      (0 until ts.length).toList.filter(i => ts(i).domain == argsTs) match{
        case List() => 
          FailureR(
            s"Application of overloaded function $fn with types\n"+
            ts.map(_.asString).mkString(", ")+"\ncan't be applied to argument"+
            (if(args.length > 1) "s" else "")+" of type "+showList(argsTs)
          ).lift(fa, true)
        case List(index) =>
          ne.setIndex(index); val range = ts(index).range
          if(range != null) Ok((te1.endScope, range)) 
          else forwardRefFail.lift(fa, true)
        case _ => 
          sys.error(s"Multiple functions $fn with arguments of type(s)"+
            showList(argsTs))
          // I think this can't happen
      }
    }
  }
}
