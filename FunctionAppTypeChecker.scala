package spreadsheet

import Unification.unify
import TypeVar.{TypeID,nextTypeID} // Type variables (Ints)
import FunctionType.TypeParameter // (TypeParamName, TypeParamConstraint)
import TypeParam.TypeParamName // Names of type parameters (Strings)
import NameExp.Name // Names of identifiers (Strings)
import TypeT.showList
import TypeChecker0.{TypeCheckRes}
import Substitution.{
  replaceTypeParamsByTypeVars, TypeMap, inverse, remapTypeVarsBackToTypeParams}

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

  /** Typecheck the application of a function of type ft to args. */
  private 
  def checkFunctionApp1(typeEnv: TypeEnv, ft: FunctionType, args: List[Exp])
      : TypeCheckRes = {
// println(s"checkFunctionApp1(\n\t$ft,\n\t$args)")
    val FunctionType(tParams, domain, range) = ft
    if(domain.length != args.length)
      FailureR(s"Expected ${domain.length} arguments, found "+args.length)
    else{
      val unusedTParams = ft.unusedTParams
      // Create fresh type variables to replace tParams in domain and range
      val (te1, domain1, range1, typeMap) =
        Substitution.replaceTypeParamsByTypeVars(
          typeEnv.newScope, ft.usedTParams, domain, range)
      // Generate a new name, and bind it to range1 in the environment;
      // then unify the types of args with domain1, so the new name gets
      // updated to the appropriate return type.
      val name = newName(); val te2 = te1 + (name, range1)
      typeCheckListUnify(te2, args, domain1).map{ te3 =>
// println(s"${te3(name)}")
        // Extract type of name, and add unusedTParams to FunctionType results
        val (te4, res) = 
          Substitution.subTypeParamsInResult(te3, unusedTParams, te3(name))
// TODO: Add tParams in res 
        Ok((te4.endScope, res))
      }
    }
  }

  /** Type check each element of es, unifying its type with the corresponding
    * element of ts. 
    * Pre: es.length == ts.length.
    * Used to check actual parameters es of a function application against the
    * expected types ts.  */
  private 
  def typeCheckListUnify(typeEnv: TypeEnv, es: List[Exp], ts: List[TypeT])
      : Reply[TypeEnv] = 
    if(es.isEmpty){ assert(ts.isEmpty); Ok(typeEnv) }
    else etc.typeCheck(typeEnv, es.head).map{ case (te1,t1) => 
      // If t1 is a FunctionType, instantiate type parameters
      val (te2,t2,typeMap) = mkInstance(te1,t1) 
// println(s"typeCheckListUnify:\n\t${es.head}: $t1\n\t -> $t2;\n\t unifying with ${ts.head} \n via $typeMap")
      // Unify with formal parameter type in ts, and recurse.
      unify(te2, t2, ts.head).lift(es.head, true).map{ case (te3, _) => 
        // Replace type parameters
        val te4 =
          if(typeMap == null) te3 
          else{
            assert(t1.isInstanceOf[FunctionType])
            val FunctionType(tParams,_,_) = t1
            val invMap = inverse(typeMap); //val tParams = t1.params // ypeMap.keys.toList
            //println(s"tParams = $tParams; t1 = $t1")
            te3.map(remapTypeVarsBackToTypeParams(invMap,tParams, _))
          }
        typeCheckListUnify(te4, es.tail, ts.tail)
      }
    }

  /** If t is a FunctionType, make an instance of it, replacing each type
    * parameter by a fresh type variable, and add suitable constraints to
    * typeEnv. */
  private def mkInstance(typeEnv: TypeEnv, t: TypeT): (TypeEnv, TypeT, TypeMap) = t match{
    case FunctionType(tParams, domain, range) => 
      // println("mkInstance: "+t)
      val (te, domain1, range1, typeMap) = 
        replaceTypeParamsByTypeVars(typeEnv, tParams, domain, range)
      (te, FunctionType(List(), domain1, range1), typeMap)
    case _ => (typeEnv, t, null)
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
          ne.setIndex(index); Ok((te1.endScope, ts(index).range))
        case _ => 
          sys.error(s"Multiple functions $fn with arguments of type(s)"+
            showList(argsTs))
          // I think this can't happen
      }
    }
  }
}
