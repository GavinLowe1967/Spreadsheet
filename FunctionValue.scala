package spreadsheet

/* This file collects together classes related to functions.  The classes are
 * subclasses of Value, Statement and Exp, but the first can't be included in
 * Value.scala, since it builds on Exp; and the last can't be included in
 * Exp.scala, because it builds on Statement. */

/** A value of a function, params => body: rt, to be evaluated in environment
  * env.   */
case class FunctionValue(
  params: List[(String,TypeT)], rt: TypeT, body: Exp, env: Environment)
    extends Value{

  protected val theType = FunctionType(List(), params.map(_._2), rt)
}

// =======================================================

/** The declaration of a function "def name[tparams](args): rt = exp". */
case class FunctionDeclaration(
  name: String, tParams: List[FunctionType.TypeParameter], 
  params: List[(String, TypeT)], rt: TypeT, body: Exp)
    extends Declaration{

  /** Perform this in `env`, handling errors with `handleError`. */
  def perform(env: Environment, handleError: ErrorValue => Unit): Boolean = {
    val fv = FunctionValue(params, rt, body, env)
    env.update(name, fv); true
  }
}

// =======================================================

/** The application of a function represented by `f` to `args`. */
case class FunctionApp(f: Exp, args: List[Exp]) extends Exp{

  def eval0(env: EnvironmentT) = {
    f.eval(env) match{
      case FunctionValue(params, rt, body, env1) =>
        assert(params.length == args.length) 
        // Try to bind params to values of args in env1; but catch errors
        val env2 = env1.cloneE; var error: ErrorValue = null
        var iter = params.zip(args).iterator
        while(error == null && iter.hasNext){
          val ((p,_),arg) = iter.next()
          arg.eval(env) match{
            case e: ErrorValue => error = liftError(e, true)
            case v =>  env2.update(p, v)
          }
        }
        if(error != null) error
        else body.eval(env2) match{
          case err: ErrorValue => liftError(err); case result => result
        }

      case f: BuiltInFunction =>
        var error: ErrorValue = null; val iter = args.iterator
        var vs = List[Value]() // built in reverse
        while(error == null && iter.hasNext){
          val arg = iter.next()
          arg.eval(env) match{
            case e: ErrorValue => error = liftError(e, true)
            case v =>  vs ::= v
          }
        }
        if(error != null) error
        else f(vs.reverse) match{
          case err: ErrorValue => liftError(err); case result => result
        }
   
      case err: ErrorValue => liftError(err)

      case other => sys.error(s"$f -> $other")
    }
  }
}
