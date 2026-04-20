package spreadsheet

import TypeT._
import TypeCheckerTest2._, TypeCheckerTest3._, TypeCheckerTest4._
//import TypeChecker.TestHooks._

/** Tests on the type checker. */
object TypeCheckerTest{
  def main(args: Array[String]) = {
    var doAll = true; var i = 0
    while(i < args.length) args(i) match{
      case "--restrict" => doAll = false; i += 1
    }

    if(doAll){
      println("===exptests===")
      // printErrors = true
      // ------                    TypeCheckerTest0.scala
      TypeCheckerTestExpr.expTests()
      // --------                  TypeCheckerTest1.scala
      println("===singleDecTests===")
      TypeCheckerTest1.singleDecTests()
      println("===scriptTests===")
      TypeCheckerTest1.scriptTests()
      println("===cellTests===")
      TypeCheckerTest1.cellTests()
      println("===cellWriteTests===")
      TypeCheckerTest1.cellWriteTests()
      println("===listTests===")
      TypeCheckerTest1.listTests()
      // -----                      TypeCheckerTest2.scala
      println("===polyTests===")
      polyTests()
      println("===concatTest===")
      concatTest()
      println("===typeConstraintTests===")
      typeConstraintTests()
      println("===higherOrderTests===")
      higherOrderTests()
      // ------                      TypeCheckerTest3.scala
      println("===forLoopTests===")
      forLoopTests()
      println("===overloadingtests===")
      overloadingTests()
      println("===curryingtests===")
      curryingTests()
      curryingTests2()
      // ------                     TypeCheckerTest4.scala
      preludeTests()
      rtTests()
      haskellTests()
      tupleTests()
      callTests()
    }
    println("Done")
  }

}
