package spreadsheet

import TypeT._
import TypeCheckerTest0._, TypeCheckerTest1._, TypeCheckerTest2._
import TypeChecker.TestHooks._

/** Tests on the type checker. */
object TypeCheckerTest{


  // ==================================================================

  // ==================================================================

  // =======================================================

  /* Some definitions of functions for use in tests. */
  val double = "def double(y: Int) : Int = 2*y\n"
  val apply = "def apply[A,B](f: A => B, x: A): B = f(x)\n"
  val map = "def map[A, B](f: A => B, xs: List[A]): List[B] = "+
    "if(isEmpty(xs)) [] else f(head(xs)) :: map(f, tail(xs))\n"
  val applyToThree = "def applyToThree[B](f: Int => B): B = f(3)\n"
  val id = "def id[A](x: A): A = x\n"


  // =======================================================

  def main(args: Array[String]) = {
    var doAll = true; var i = 0
    while(i < args.length) args(i) match{
      case "--restrict" => doAll = false; i += 1
    }


    if(doAll){
      println("===exptests===")
      // printErrors = true
      TypeCheckerTestExpr.expTests()
      println("===singleDecTests===")
      singleDecTests()
      println("===scriptTests===")
      scriptTests()
      println("===cellTests===")
      cellTests()
      println("===cellWriteTests===")
      cellWriteTests()
      println("===listTests===")
      listTests()
      println("===polyTests===")
      polyTests()
      println("===typeConstraintTests===")
      typeConstraintTests()
      println("===higherOrderTests===")
      higherOrderTests()
    }
    println("Done")
  }

}
