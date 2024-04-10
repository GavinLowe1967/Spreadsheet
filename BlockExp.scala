package spreadsheet

/* Note: we can't include this in Exp.scala, because it builds on Statement. */

/** A block expression of the form { stmt_1; ...; stmt_n; exp }. */ 
case class BlockExp(stmts: List[Declaration], exp: Exp) extends Exp{
  // def eval0(env: EnvironmentT) = {
  //   val env1 = env.cloneE; // var ok = true; val iter = stmts.iterator
  //   // If an error arises in performing stmts, it will be put in err.
  //   var err: ErrorValue = null
  //   def handleError(ev: ErrorValue) = { err = ev }
  //   val ok = Statement.performAll(stmts, env1, handleError)
  //   // while(ok && iter.hasNext) ok = iter.next().perform(env1, handleError)
  //   if(ok){
  //     assert(err == null)
  //     exp.eval(env1) match{
  //       case ev: ErrorValue => liftError(ev)
  //       case res => res
  //     }
  //   }
  //   else  liftError(err) 
  // }
}
