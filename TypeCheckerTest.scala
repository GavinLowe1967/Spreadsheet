package spreadsheet

import TypeT._
import TypeCheckerTestDecl._, TypeCheckerTest1._, TypeCheckerTest2._, 
  TypeCheckerTest3._, TypeCheckerTest4._, TypeCheckerTest5._, 
  TypeCheckerConstraintTest._
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
      // --------                   TypeCheckerTestDecl.scala
      println("===singleDecTests===")
      singleDecTests()

      println("===scriptTests===")
      scriptTests()
      println("===listTests===")
      listTests()
      // --------                  TypeCheckerTest1.scala
      assertionTests()
      println("===cellTests===")
      cellTests()
      println("===cellWriteTests===")
      cellWriteTests()
      println("===typeParamTests===")
      typeParamTests()
      // -----                      TypeCheckerTest2.scala
      println("===polyTests===")
      polyTests()
      println("===concatTest===")
      concatTest()
      println("===typeConstraintTests===")
      typeConstraintTests()
      println("===higherOrderTests===")
      higherOrderTests()
      // -----                       TypeCheckerConstraintTest.scala
      numTests()

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
      minimumTest()
      rtTests()
      haskellTests()
      // -----                      TypeCheckerTest5.scala
      tupleTests()
      callTests()
      ifStmtTests()
      operationTests()
    }
    println("Done")
  }

}
