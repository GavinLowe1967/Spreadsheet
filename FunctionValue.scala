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

  protected val theType = FunctionType(params.map(_._2), rt)
}

// =======================================================

/** The declaration of a function "def name(args): rt = exp". */
case class FunctionDeclaration(
  name: String, params: List[(String, TypeT)], rt: TypeT, body: Exp) 
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
  /** Produce error: expected m arguments, found n. */
  private def mkLengthError(m: Int, n: Int) = 
    liftError(EvalError(
      s"Expected $m argument"+(if(m != 1) "s" else "")+s", found $n"
    ))

  def eval0(env: Environment) = {
    f.eval(env) match{
      case FunctionValue(params, rt, body, env1) =>
        if(params.length != args.length) 
          mkLengthError(params.length, args.length)
        else{
          // Try to bind params to values of args in env1; but catch errors
          val env2 = env1.clone; var error: ErrorValue = null
          var iter = params.zip(args).iterator
          while(error == null && iter.hasNext){
            val ((p,t),arg) = iter.next()
            arg.eval(env) match{
              case e: ErrorValue => error = liftError(e)
              case v => 
                if(v.isOfType(t)) env2.update(p, v) 
                else error = mkTypeError(t.asString, v) 
            }
          }
          if(error != null) error 
          else body.evalExpectType(env2, rt) match{
            case err: ErrorValue => liftError(err)
            case result => assert(result.isOfType(rt)); result //FIXME: assertion
          }
        }

      case f: BuiltInFunction => 
        val paramTs = f.paramTypes
        if(paramTs.length != args.length)
          mkLengthError(paramTs.length, args.length)
        else{
          // Evaluate args, checking against parameter types of f
          val iter = f.paramTypes.zip(args).iterator
          var error: ErrorValue = null
          var vs = List[Value]() // built in reverse
          while(error == null && iter.hasNext){
            val (t,arg) = iter.next()
            arg.eval(env) match{
              case e: ErrorValue => error = liftError(e)
              case v =>
                if(v.isOfType(t)) vs ::= v
                else error = mkTypeError(t.asString, v)
            }
          }
          if(error != null) error 
          else f(vs.reverse) match{
            case err: ErrorValue => liftError(err)
            case result => assert(result.isOfType(f.rt)); result
          }
        }

      case err: ErrorValue => liftError(err)

      case other => println(s"$f -> $other"); ??? // FIXME
    }
  }
}
