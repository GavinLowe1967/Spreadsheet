package spreadsheet

/** The type checker to use during evaluation. */
object EvaluationTypeChecker{
  import TypeVar.TypeID // Type variables (Ints)

  /** Make a Failure Repl, for use in unify and TypeChecker.unify. */
  def mkFailure(typeEnv: EvaluationTypeEnv, t1: TypeT, t2: TypeT) = 
    FailureR(s"Expected "+typeEnv.showType(t2)+", found "+typeEnv.showType(t1))

  /** Replace tId by t in typeEnv, if it is consistent with the constraints in
    * typeEnv. Otherwise return fail. */
  // private def replaceInTypeEnv(
  //   typeEnv: EvaluationTypeEnv, tId: TypeID, t: CellType, fail: => FailureR)
  //     : Reply[EvaluationTypeEnv] = {
  //   // assert(!t.isInstanceOf[TypeVar])
  //   // println(s"replaceInTypeEnv: Cell type $t, type constraint "+typeEnv(tId))
  //   if(typeEnv(tId).satisfiedBy(typeEnv, t)) 
  //     Ok(typeEnv.replaceEvalTime(tId, t))
  //   else fail
  // }

  /** Try to unify t1 and t2, at runtime.  Do not update any typing in cells
    * (within typeEnv.replace).  t1 is the type of a value read from a cell.
    * t2 is the type that the typechecker associated with an expression that
    * reads the cell.  Pre: t1 is a concrete type (but t2 might be a
    * TypeVar). */
  def unify(typeEnv: EvaluationTypeEnv, t1: CellType, t2: TypeT)
      : Reply[EvaluationTypeEnv] = {
    def fail = mkFailure(typeEnv, t1, t2)
    // If t2 is associated with a constraint that allows both Ints and Floats,
    // and t1 is one of those types, we subsequently associate t2 with either
    // of those types.
    t2 match{
      case TypeVar(tId2) => typeEnv(tId2) match{
        case MemberOf(ts) if ts.contains(IntType) && ts.contains(FloatType) && 
            (t1 == IntType || t1 == FloatType) =>
          Ok(typeEnv.replaceEvalTime(tId2, NumTypeConstraint))
        case tc =>
          // replaceInTypeEnv(typeEnv, tId2, t1, fail)
          if(tc.satisfiedBy(typeEnv, t1)) Ok(typeEnv.replaceEvalTime(tId2, t1))
          else fail
      }
      case _ => if(t1 == t2) Ok(typeEnv) else fail
    }
  }

/*
        case NumTypeConstraint => // don't update here
          if(t1 == IntType || t1 == FloatType) Ok(typeEnv) else fail
 */
/*
        case EqTypeConstraint => t1 match{
          case IntType | FloatType =>
            Ok(typeEnv.replaceEvalTime(tId2, NumTypeConstraint))
          case _ => replaceInTypeEnv(typeEnv, tId2, t1, fail)
        }
 */
// IMPROVE: something similar for EqTypeConstraint ???
}
