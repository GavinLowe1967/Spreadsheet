package spreadsheet

/** The type checker to use during evaluation. */
object EvaluationTypeChecker{
  import TypeVar.TypeID // Type variables (Ints)

  /** Make a Failure Repl, for use in unify and TypeChecker.unify. */
  def mkFailure(typeEnv: TypeEnv, t1: TypeT, t2: TypeT) = 
    FailureR(s"Expected "+typeEnv.showType(t2)+", found "+typeEnv.showType(t1))

  /** Replace tId by t in typeEnv, if it is consistent with the constraints in
    * typeEnv. Otherwise return fail. */
  private def replaceInTypeEnv(
    typeEnv: TypeEnv, tId: TypeID, t: TypeT, fail: => FailureR)
      : Reply[(TypeEnv, TypeT)] =
    if(typeEnv(tId).satisfiedBy(typeEnv, t)) 
      Ok(typeEnv.replaceEvalTime(tId, t), t)
    else fail

  /** Try to unify t1 and t2, at runtime.  Do not update any typing in cells
    * (within typeEnv.replace).  Pre: t1 is a concrete type (but t2 might be a
    * TypeVar). */
  def unify(typeEnv: TypeEnv, t1: TypeT, t2: TypeT): Reply[(TypeEnv, TypeT)] = {
    def fail = mkFailure(typeEnv, t1, t2)
    require(!t1.isInstanceOf[TypeVar])
    t2 match{
      case TypeVar(tId2) => replaceInTypeEnv(typeEnv, tId2, t1, fail)
      case _ => if(t1 == t2) Ok(typeEnv, t2) else fail
    }
  }

}
