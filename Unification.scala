package spreadsheet

/** Unification of types. */
object Unification{

  /** Make a Failure Repl, for use in unify and TypeChecker.unify. */
  def mkFailure(typeEnv: EvaluationTypeEnv, t1: TypeT, t2: TypeT) = 
    FailureR(s"Expected "+typeEnv.showType(t2)+", found "+typeEnv.showType(t1))

  import TypeVar.TypeID // Type variables (Ints)

  /** Replace tId by t in typeEnv, if it is consistent with the constraints in
    * typeEnv. Otherwise return fail. */
  private def replaceInTypeEnv(
    typeEnv: TypeEnv, tId: TypeID, t: TypeT, fail: => FailureR)
      : Reply[(TypeEnv, TypeT)] = {
    assert(!t.isInstanceOf[TypeVar])
    updateEnvToSatisfy(typeEnv, t, typeEnv(tId), fail).map{ te => 
      Ok(te.replace(tId, t), t)
    }
  }

  /** Test whether t can satisfy the constraint c.  If needs be, add constraints
    * to TypeVars within t.  Return the resulting environment if successful;
    * otherwise return fail. */
  private def updateEnvToSatisfy(
    typeEnv: TypeEnv, t: TypeT, c: TypeConstraint, fail: => FailureR)
      : Reply[TypeEnv] = {
    if(verbose) println(s"updateEnvToSatisfy($t, $c)")
    t match{
      case ListType(underlying) => c match{
        case EqTypeConstraint => updateEnvToSatisfy(typeEnv, underlying, c, fail)
        case AnyTypeConstraint => Ok(typeEnv)
      }
      case _ : FunctionType => c match{
        case AnyTypeConstraint => Ok(typeEnv)
        case EqTypeConstraint => fail
      }
      case TypeVar(tId) => 
        // This can happen by recursing via ListType(TypeVar(_)), e.g. the
        // test "equals([],[[]])" in polyListTests.
        val c1 = typeEnv(tId); val cc = c.intersection(typeEnv, c1)
        if(cc == c1) Ok(typeEnv) else Ok(typeEnv + (tId,cc))
      case _ => // BaseTypes, TypeParam
        if(c.satisfiedBy(typeEnv, t)) Ok(typeEnv) else fail
    }
  }

  /** Identity on types. */
  private val idT = (t: TypeT) => t 

  var verbose = false

  /** Try to unify t1 and t2.  If successful, return updated typeEnv and unified
    * type.  t2 is expected to be the "expected" type, and t1 the type that is
    * being matched against it. 
    * @param f a function that transforms the way types are reported in errors. 
    */
  def unify(typeEnv: TypeEnv, t1: TypeT, t2: TypeT, f: TypeT => TypeT = idT)
      : Reply[(TypeEnv, TypeT)] = {
    if(verbose) println(s"\nunify($typeEnv,\n$t1, $t2)")
    def fail = mkFailure(typeEnv, f(t1), f(t2)) 
    if(t1 == t2) Ok(typeEnv, t1)
    else (t1,t2) match{
      case (TypeVar(tId1), TypeVar(tId2)) => // Both TypeVars: find intersection
        //if(true || verbose) println(s"\nunify($t1, $t2)")
        typeEnv(tId1).intersection(typeEnv, typeEnv(tId2)) match{
          case SingletonTypeConstraint(t) =>
            val newTypeEnv = typeEnv.replace(tId1, t).replace(tId2, t)
            Ok(newTypeEnv, t)
          case cc => // EqTypeConstraint or AnyTypeConstraint
            // assert(false)
            if(verbose) println(typeEnv(tId1).toString+" \t"+typeEnv(tId2))
          // replace t2 by t1, with constraint cc
            Ok(typeEnv.replace(tId2, t1) + (tId1, cc), t1)
        }

      case (TypeVar(tId1), TypeParam(tp)) => fail
        // The above happens in cases like "def mkEmpty[A](x: A): List[A] =
        // []", which leads to an attempt to unify ListType(TypeVar(tId1))
        // against ListType(TypeParam("A")), which recurses here.

      case (TypeVar(tId1), _) => // t2 a concrete type.  Try to replace t1 by t2
        replaceInTypeEnv(typeEnv, tId1, t2, fail)
        
      case ( _, TypeVar(tId2)) =>           // Try to replace t2 by t1
        // if(t1.isInstanceOf[ListType]) 
        //   println(s"\nunify($t1, $t2 <: ${typeEnv(tId2)})")
        assert(!t1.isInstanceOf[TypeVar])
        replaceInTypeEnv(typeEnv, tId2, t1, fail)

      case (ListType(tt1), ListType(tt2)) => 
        unify(typeEnv, tt1, tt2, ListType(_)).map{ 
          case (te2, tt) => Ok((te2, ListType(tt))) 
        }
        // Note: if the recursive call fails, the error message talks about
        // ListType(tt1) and ListType(tt2).

      case (ct @ CellTypeVar(tv), t: CellType) => 
        // println(s"$tv $t"); 
        Ok((typeEnv + (ct,t), t))

      case (_, TypeParam(tp)) =>  fail

      case (FunctionType(tc1,d1,r1), FunctionType(tc2,d2,r2)) =>  
// TODO: is the following true?
        assert(tc1.isEmpty && tc2.isEmpty) // ???????????
        //println(s"Unifying $t1\n and $t2\n")
        unifyList(typeEnv, d1, d2).map{ case (te1, dd) =>
          unify(te1, r1, r2).map{ case (te2, rr) =>
            Ok((te2, FunctionType(List(), dd, rr)))
          }
        }

      case (_,_) => fail
    }
  }

  /** Unify corresponding elements of the lists ts1 and ts2. */
  private def unifyList(typeEnv: TypeEnv, ts1: List[TypeT], ts2: List[TypeT])
      : Reply[(TypeEnv, List[TypeT])] =
    if(ts1.isEmpty){ assert(ts2.isEmpty); Ok((typeEnv, List())) }
    else unify(typeEnv, ts1.head, ts2.head).map{ case (te1, t1) => 
      unifyList(te1, ts1.tail, ts2.tail).map{ case (te2, ts) => 
        Ok((te2, t1::ts))
      }
    }

}
