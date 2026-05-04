package spreadsheet

import Unification.unify
import TypeVar.{TypeID,nextTypeID} // Type variables (Ints)
import FunctionType.TypeParameter  // (TypeParamName, TypeParamConstraint)
import TypeParam.TypeParamName   // Names of type parameters (Strings)
import NameExp.Name // Names of identifiers (Strings)
import TypeT.showList
import TypeChecker0.{TypeCheckRes}
import TypeParamSubstitution.{
  replaceTypeParamsByTypeVars, TypeMap, emptyTypeMap, ReverseTypeMap, inverse, 
  union, emptyRevMap, reverseRemapBy, subTypeParamsInResult}

/** Type checking of function applications. */
class FunctionAppTypeChecker(etc: ExpTypeCheckerT){
  /** Counter for generating new names. */
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
    "Function without explicit return type applied recursively, \n"+
      "or before its definition,")

  /** Typecheck the application of a function of type ft to args. */
  private 
  def checkFunctionApp1(typeEnv: TypeEnv, ft: FunctionType, args: List[Exp])
      : TypeCheckRes = {
// println(s"***\ncheckFunctionApp1(\n\t$ft,\n\t$args)")
    val FunctionType(tParams, domain, range) = ft
    if(domain.length != args.length)
      FailureR(s"Expected ${domain.length} arguments, found "+args.length)
    else if(ft.finalNull) forwardRefFail
    else{
      // Create fresh type variables to replace tParams in domain and range
      val (te1, domain1, range1, typeMap) =
        replaceTypeParamsByTypeVars(
          typeEnv.newScope, ft.usedTParams, domain, range)
      checkFunctionApp2(te1, domain1, range1, ft.typeParams.toSet, args).map{
        case (te2, res0) => 
// println(s"res0 = $res0")
          val (te3, res) = subTypeParamsInResult(te2, ft.unusedTParams, res0)
          Ok((te3.endScope, res))
//          Ok((te2.endScope, res0))
      }
    }
  }

  /** Type check function of type domain => range to arguments args.  If
    * successful, return the resulting environment and the type of the
    * application.  `typeParams` is the type parameters bound in the type of
    * the function.  */
  private def checkFunctionApp2(
    typeEnv: TypeEnv, domain: List[TypeT], range: TypeT, 
    typeParams: Set[TypeParamName], args: List[Exp])
      : TypeCheckRes = {
// println(s"checkFunctionApp2(\n\t domain = $domain,\n\t range = $range, \n\t args = $args)")
    // Generate a new name, and bind it to range in the environment;
    // then unify the types of args with domain, so the new name gets
    // updated to the appropriate return type.
    val name = newName(); val te1 = typeEnv + (name, range)
    typeCheckList1(te1, typeParams, args).map{ case (te2, argTs) =>
// println(s"$args -> $argTs")
//println(s"te2 = $te2")
      unifyList(te2, argTs, domain).map{ case (te3, invMap) =>
        // Extract type of name.  Need to apply invMap to reverse renaming
        // done in unifyList.
//println(s"te3 = $te3")
//println(s"name: "+te3(name))
//println(s"invMap = $invMap")
        Ok((te3, reverseRemapBy(invMap, te3(name))))
      }
    }
  }

  /** Type check each element of args.  Rename bound type parameters to avoid
    * clashes with elements of fnTParams.  If successful, return resulting
    * environment, and list of types. */
  private def typeCheckList1(
    typeEnv: TypeEnv, fnTParams: Set[TypeParamName], args: List[Exp])
      : Reply[(TypeEnv, List[TypeT])] = {
//println(s"typeCheckList1: ${args}")
    if(args.isEmpty) Ok((typeEnv, List[TypeT]()))
    else etc.typeCheck(typeEnv, args.head).map{ case (te1, t1) =>
//println(s"typeCheckList1: ${args.head}: $t1")
      if(t1.hasNullReturnFunction) forwardRefFail 
      else{
        // Rename type parameters to avoid name clashes.
        val t2 = t1.renameTypeParams(/*TypeParam.newTypeParamMap,*/ fnTParams)
//println(s"typeCheckList1: $t1 -> $t2")
        typeCheckList1(te1, fnTParams, args.tail).map{ case (te2, ts) =>
          Ok(te2, t2::ts)
        }
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
      // println(s"Unifying $t1 and\n${paramTs.head}")
      unify(te1, t1, paramTs.head).map{ case (te2, _) =>
        unifyList(te2, argTs.tail, paramTs.tail).map{ case (te3, invMap) =>
          Ok((te3, union(invMap, inverse(typeMap))))
        }
      }
    }

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

  def findFunctionApp(typeEnv: TypeEnv, fa: FunctionApp, ts: Array[FunctionType])
      : Reply[(TypeEnv, TypeT)] = {
    val FunctionApp(ne @ NameExp(fn), args) = fa
    assert(ts.length >= 2, ts.mkString("\n"))
    // If args don't typecheck, then just return the error
    etc.typeCheckList(typeEnv.newScope, args).lift(fa).map{ case (_,argsTs) => 
      // Iterate through ts.  Note: this involves typechecking args again,
      // multiple times.  But I think this is unavoidable, as in each later
      // case we need to consider the relevant type parameters.
      var i = 0; var result: Reply[(TypeEnv, TypeT)] = null
      var errs = List[String]()
      while(i < ts.length && result == null){
        val ft = ts(i)
        checkFunctionApp1(typeEnv, ft, args) match{
          case ok @ Ok((te,res)) => ne.setIndex(i); result = ok
          case FailureR(err) => 
            //if(!ft.finalNull) 
              errs = errs :+ s"Instance of type ${ft.asString}: $err"
            i += 1
        }
      } // end of while loop
      if(result != null) result 
      else FailureR(
        s"Overloaded function $fn can't be applied to argument"+
            // with types\n"+
            // ts.map(_.asString).mkString(", ")+
            //"\ncan't be applied to argument"+
          (if(args.length > 1) "s" else "")+" of type "+showList(argsTs)+"\n"+
          errs.mkString("\n")+"\n"
      ).lift(fa, true)
    }
  }
}
