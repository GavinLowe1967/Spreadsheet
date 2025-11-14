package spreadsheet

import Unification.unify
import TypeVar.TypeID // Type variables (Ints)
import FunctionType.TypeParameter // (TypeParamName, TypeParamConstraint)
import TypeParam.TypeParamName // Names of type parameters (Strings)
import NameExp.Name // Names of identifiers (Strings)
import TypeT.showList
import TypeChecker0.{TypeCheckRes,nextTypeID}

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
    val FunctionType(tParams, domain, range) = ft
    if(domain.length != args.length)
      FailureR(s"Expected ${domain.length} arguments, found "+args.length)
    else{
      // Create fresh type variables to replace tParams in domain and range
      val (te1, domain1, range1) =
        subTypeParams(typeEnv.newScope, tParams, domain, range)
      // Generate a new name, and bind it to range1 in the environment;
      // then unify the types of args with domain1, so the new name gets
      // updated to the appropriate return type.
      val name = newName(); val te2 = te1 + (name, range1)
      typeCheckListUnify(te2, args, domain1).map{ te3 =>
        Ok((te3.endScope, te3(name)))  // extract type of name
      }
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
      val (te2,t2) = mkInstance(te1,t1) 
      // Unify with formal parameter type in ts, and recurse.
      unify(te2, t2, ts.head).lift(es.head, true).map{ case (te3, _) => 
        typeCheckListUnify(te3, es.tail, ts.tail)
      }
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

  /** Type check es in turn.  If successful, return the resulting type
    * environment and list of types. */
  private def typeCheckList(typeEnv: TypeEnv, es: List[Exp])
      : Reply[(TypeEnv, List[TypeT])] = {
    if(es.isEmpty) Ok(typeEnv, List[TypeT]())
    else etc.typeCheck(typeEnv, es.head).map{ case(te1, t1) => 
      val (te2,t2) = mkInstance(te1,t1)
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
