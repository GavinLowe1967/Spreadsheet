package spreadsheet

/** Unification of types. */
object Unification{

  import EvaluationTypeChecker.mkFailure
  import TypeVar.TypeID // Type variables (Ints)

  /** Replace tId by t in typeEnv, if it is consistent with the constraints in
    * typeEnv. Otherwise return fail. */
  private def replaceInTypeEnv(
    typeEnv: TypeEnv, tId: TypeID, t: TypeT, fail: => FailureR)
      : Reply[(TypeEnv, TypeT)] = {
    assert(!t.isInstanceOf[TypeVar])
    if(typeEnv(tId).satisfiedBy(typeEnv, t)) Ok(typeEnv.replace(tId, t), t)
    else fail
  }

  /** Identity on types. */
  private val idT = (t: TypeT) => t 

  /** Try to unify t1 and t2.  If successful, return updated typeEnv and unified
    * type.  t2 is expected to be the "expected" type, and t1 the type that is
    * being matched against it. 
    * @param f a function that transforms the way types are reported in errors. 
    */
  def unify(typeEnv: TypeEnv, t1: TypeT, t2: TypeT, f: TypeT => TypeT = idT)
      : Reply[(TypeEnv, TypeT)] = {
    // println(s"unify($typeEnv,\n$t1, $t2)")
    def fail = mkFailure(typeEnv, f(t1), f(t2)) 
    if(t1 == t2) Ok(typeEnv, t1)
    else (t1,t2) match{
      case (TypeVar(tId1), TypeVar(tId2)) => // Both TypeVars: find intersection
        typeEnv(tId1).intersection(typeEnv, typeEnv(tId2)) match{
          case EmptyTypeConstraint =>  fail
          case SingletonTypeConstraint(t) =>
            val newTypeEnv = typeEnv.replace(tId1, t).replace(tId2, t)
            Ok(newTypeEnv, t)
          case cc : StoredTypeConstraint =>
            // replace t2 by t1, with constraint cc
            Ok(typeEnv.replace(tId2, t1) + (tId1, cc), t1)
                                                      // TODO: test with ts1!=ts2
        }

      case (TypeVar(tId1), TypeParam(tp)) => 
        val c1 = typeEnv(tId1); val c2 = typeEnv.constraintForTypeParam(tp)
        println(s"Unify: $tId1 -> $c1; $tp -> $c2"); 
        fail
        // Note: the TypeParam(tp) represents a *universal* quantification
        // over at least two types.  The TypeVar(tId1) cannot simultaneously
        // have all of those types.  In particular, tId1 is associated with an
        // *existential* quantification over one or more types.

      case (TypeVar(tId1), _) => // t2 a concrete type.  Try to replace t1 by t2
        replaceInTypeEnv(typeEnv, tId1, t2, fail)
        
      case ( _, TypeVar(tId2)) =>           // Try to replace t2 by t1
        assert(!t1.isInstanceOf[TypeVar])
        replaceInTypeEnv(typeEnv, tId2, t1, fail)

      case (ListType(tt1), ListType(tt2)) => 
        unify(typeEnv, tt1, tt2, ListType(_)).map{ 
          case (te2, tt) => Ok((te2, ListType(tt))) 
        }
        // Note: if the recursive call fails, the error message talks about
        // ListType(t1) and ListType(t2).

      // case (TypeParam(tp1), TypeParam(tp2)) => 
      //   println(s"$tp1 $tp2");  fail

      case (_, TypeParam(tp)) => fail // println(s"$t1 $t2"); ???
        // Check that t1 satisfies the type constraints on tp

      case (FunctionType(List(),d1,r1), FunctionType(List(),d2,r2)) =>  
                                // TODO: test.  The "List()"s look odd
        // println(s"Unifying $t1, $t2")
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
