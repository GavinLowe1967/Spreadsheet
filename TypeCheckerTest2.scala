package spreadsheet

import TypeT._
import TypeCheckerTest0._

object TypeCheckerTest2{

  // The expected type of the identity function
  private val idFunctionType = FunctionType(
      List(("A",AnyTypeConstraint)), List(TypeParam("A")), TypeParam("A") )

  // Some test definitions
  private val idS = "def id[A](x: A) : A = x\n"
  private val applyS = "def apply[A, B](f: A => B, x: A) : B = f(x)\n"
  private val mapS = "def map[A, B](f: A => B, xs: List[A]): List[B] = "+
    "if(isEmpty(xs)) [] else f(head(xs)) :: map(f, tail(xs))\n"
  private val applyToThreeS = "def applyToThree[B](f: Int => B): B = f(3)\n"
  private val doubleS = "def double(y: Int) : Int = 2*y\n"
  private val threeS = "def three[A](x: A): Int = 3\n"
  private val applyXS = "def applyX[A](f: A => Int, x: A): Int = f(x)\n"
  private val script0 = applyS+doubleS+mapS+applyToThreeS+idS+threeS+applyXS

  // ==========

  /** Tests on polymorphic functions. */
  def polyTests() = {
    // Sanity checks on parameters
    assertFail(tcpss("def f[A](x: A, x: A): Int = 0")) // repeated params
    assertFail(tcpss("def f[A, A](x: Int): Int = 0")) // repeated tparams
    assertFail(tcpss("def f(x: A): Int = 0")) // missing tparam

    tcpss("def add[A](x: Int, y: Int) : Int = x+y") match{ case Ok(te) => 
      assert(te("add") == FunctionType(
        List(("A",AnyTypeConstraint)), List(IntType, IntType), IntType ) )
    }
    tcpss(idS) match{ case Ok(te) => assert(te("id") ==  idFunctionType) }
    tcpss(applyS) match{ case Ok(te) =>
      assert(te("apply") == FunctionType(
        List(("A",AnyTypeConstraint), ("B",AnyTypeConstraint)),
        List(
          FunctionType(List(), List(TypeParam("A")), TypeParam("B")), // f
          TypeParam("A") ), // x
        TypeParam("B") ) )
    }
    tcpss(idS+"val y = id(3)") match{ case Ok(te) => assert(te("y") == IntType) }

    assertFail(tcpss("def id[A,B](x: A) : B = x\n"))
    // Arguably the following should pass: it does in Scala
    assertFail(tcpss("def mkEmpty[A](x: A): List[A] = []"))
    tcpss("def mkSingle[A](x: A): List[A] = [x]") match{ case Ok(te) => 
      assert(te("mkSingle") == FunctionType(
        List(("A",AnyTypeConstraint)), 
        List(TypeParam("A")), ListType(TypeParam("A")) ) )}

    val script = applyS+"def g(x: Int): Boolean = x > 2"
    tcpss(script+"; val b = apply(g, 4)") match{ case Ok(te) => 
      assert(te("b") == BoolType) }
    assertFail(tcpss("def f[A](x: A, y: A): A = x; val y = f(3, true)"))
    assertFail(tcpss(script+"; val c = apply(g, true)"))

    // Tests where constraints are not satisfied.
    assertFail(tcpss("def f[A](x: A): Int = x")) // A doesn't match Int
    assertFail(tcpss("def f[A](x: A): Boolean = x > 3")) // ">" not def-ed on A
    assertFail(tcpss("def f[A](x: A): Boolean = x == x")) // A doesn't match Eq
    assertFail(tcpss("def f[A](x: A): A = x+x")) // "+" not def-ed on A
    assertFail(tcpss("def f[A <: Eq](x: A): A = x+x")) // "+" not def-ed on A
    assertFail(tcpss("def f[A](x: A): A = 3+x"))// "+" not def-ed on A
    assertFail(tcpss("def f[A, B](x: A): B = x")) // B is not A
    assertFail(tcpss("def f[A](x: A): A = #B3: Int")) // Int is not A
    assertFail(tcpss("def f(x: Float): Float = if(true) x else #B3: Int")) // Int not Float
    assertFail(tcpss("def f[A](x: A): A = if(true) x else #B3: Int"))

    // Tests using nested functions
    nestedPolyTests()
    // Tests on lists
    polyListTests()
  }

  /** Tests using nested polymorphic functions. */
  private def nestedPolyTests() = {
    println("=nestedPolyTests=")
    val script1 = "def f[A](x: A): A = { def g(y: A): A = y; g(x) }"
    tcpss(script1) match{ case Ok(te) => assert(te("f") == idFunctionType) }

    val script2 = "def f[A](x: A): A = { def g[A](y: A): A = y; g(x) }"
    tcpss(script2) match{ case Ok(te) => assert(te("f") == idFunctionType) }

    val script3 = "def f[A](x: A): A = { def g[B](y: B): B = y; g(x) }"
    tcpss(script3) match{ case Ok(te) =>  assert(te("f") == idFunctionType) }

    val script4 = "def f[A](x: A): A = { def g[B](y: B): A = y; x }" 
    assertFail(tcpss(script4)) // A and B don't match

    val script5 = "def f[A](x: A): A = { def g(y: Int): Int = y; g(x) }"
    assertFail(tcpss(script5)) // A doesn't match Int 

    val script6 = "def f(x: Int): Int = { def g[A](y: A): A = y; g(x) }"
    tcpss(script6) match{ case Ok(te) => // But Int matches A 
      assert(te("f") == FunctionType(List(), List(IntType), IntType))
    }

    tcpss("def f[A](x: A): Int = { def g[A](y: A) : A = y; g(2) }") match{
      case Ok(te) => 
        assert(te("f") == FunctionType(
          List(("A",AnyTypeConstraint)), List(TypeParam("A")), IntType ) )
    }

    // Below, the type parameter "A" of g is different from the type parameter
    // "A" of f.  The former gets instantiated with Boolean, and the latter
    // with Int.
    val script7 = 
      "def f[A](x: A): A = { \n"+
      "  def g[A](y: A) : A = y; def h(z: A): A = z; if(g(true)) h(x) else x \n"+
      "}; val w = f(3)"
    tcpss(script7) match{ case Ok(te) => 
      assert(te("f") == idFunctionType); assert(te("w") == IntType)
    }
  }

  def polyListTests() = {
    println("=polyListTests=")
    val lengthS = "def length[A](xs: List[A]): Int = "+
        "if(isEmpty(xs)) 0 else 1+length(tail(xs))\n"
    val equalsS = "def equals[A <: Eq](xs: List[A], ys: List[A]): Boolean = "+
      "if(isEmpty(xs)) isEmpty(ys) "+
      "else not(isEmpty(ys)) && head(xs)==head(ys) && "+
      "         equals(tail(xs),tail(ys))\n"

    // Types of defined functions
    tcpss(lengthS) match{ case Ok(te) =>
      assert(te("length") == FunctionType(
        List(("A",AnyTypeConstraint)), List(ListType(TypeParam("A"))), IntType) 
      ) }
    tcpss(equalsS) match{ case Ok(te) =>
      assert(te("equals") == FunctionType(
        List(("A",EqTypeConstraint)), 
        List(ListType(TypeParam("A")), ListType(TypeParam("A"))),
        BoolType) ) }

    // Tests on length
    tcpss(lengthS+"val n = length([true]) + length([]) + length([[]])") match{
      case Ok(te) => assert(te("n") == IntType) }

    // Tests on equals
    val script1 = 
      equalsS+"val b = equals([],[true]) && equals([], []) && "+
        "equals([4.3], []) && equals([],[[]])"
    tcpss(script1) match{ case Ok(te) => assert(te("b") == BoolType) }
    // Following gives "Expected List(Int), found List(Float) ...". 
    assertFail(tcpss(equalsS+"val b = equals([4], [3.4])"))
  }

  // ==========

  /** Tests using type constraints. */
  def typeConstraintTests() = {
    val equalS = "def equal[A <: Eq](x: A): Boolean = x == x\n"

    // Simple test on equal
    tcpss(equalS+"val y = equal(3); val z = equal(#B4: Int)") match{ 
      case Ok(te) =>
        assert(te("equal") == FunctionType(
          List(("A", EqTypeConstraint)), List(TypeParam("A")), BoolType ) )
        assert(te("y") == BoolType); assert(te("z") == BoolType)
    }

    // Functions are not Eq types
    assertFail(tcpss(equalS+idS+"val z = equal(id)"))

    // Correct use of Eq
    val script1 =
      "def f[A <: Eq](x: A): Boolean = g(x) == x; def g[B](x: B): B = x"
    tcpss(script1) match{ case Ok(te) =>
      assert(te("f") == FunctionType(
        List(("A", EqTypeConstraint)), List(TypeParam("A")), BoolType)
      )}
    tcpss(equalS+"def f[A <: Eq](x: A): Boolean = equal(x)") match{case Ok(te) =>
        assert(te("f") == FunctionType(
          List(("A",EqTypeConstraint)), List(TypeParam("A")), BoolType ) )
    }

    // Following lacking Eq constraint on type parameter.
    assertFail(tcpss("def g[B](y: B): Boolean = y == y"))
    assertFail(tcpss(equalS+"def f[A](x: A): Boolean = equal(x)"))

    val script15 = 
      "def f[A](x: A): A = x; def g[A <: Eq](x: A): Boolean = f(x) == f(x); "+
        "val z = g(2)"
    tcpss(script15) match{ case Ok(te) => assert(te("z") == BoolType) }
    // Following fails because A (in f) doesn't satisfy Eq.
    val script16 = 
      "def f[A](x: A): Boolean = g(x); def g[A <: Eq](x: A): Boolean = x == x;"+
        "val z = f(2)"
    //println(tcpss(script16))
    assertFail(tcpss(script16))
  }

  // =======================================================

  /** Tests using higher-order functions. */
  def higherOrderTests() = {
    // Check types of functions in script0
    tcpss(script0) match{ case Ok(te) => 
      assert(te("apply") == FunctionType(
        List(("A",AnyTypeConstraint), ("B",AnyTypeConstraint)),
        List(FunctionType(List(), List(TypeParam("A")), TypeParam("B")), 
          TypeParam("A")),
        TypeParam("B") ))
      assert(te("map") == FunctionType(
        List(("A",AnyTypeConstraint), ("B",AnyTypeConstraint)),
        List(FunctionType(List(), List(TypeParam("A")), TypeParam("B")), 
          ListType(TypeParam("A"))),
        ListType(TypeParam("B"))) )
      assert(te("applyToThree") == FunctionType(
        List(("B",AnyTypeConstraint)),
        List(FunctionType(List(), List(IntType), TypeParam("B"))),
        TypeParam("B")) )
      assert(te("applyX") == FunctionType(
        List(("A",AnyTypeConstraint)),
        List(FunctionType(List(), List(TypeParam("A")), IntType),
          TypeParam("A")),
        IntType) )
      assert(te("three") == FunctionType(
        List(("A",AnyTypeConstraint)), List(TypeParam("A")), IntType) )
    }
    higherOrderTests1()
    higherOrderTests2()
  }

  // ==========

  /** Applications of some functions from script0. */
  private def higherOrderTests1() = {
    // apply
    tcpss(script0+"val z = apply(double, 2)") match{ case Ok(te) => 
      assert(te("z") == IntType) 
    }
    // map (double, _)
    tcpss(script0+"val xs = map(double, [1,2])") match{ case Ok(te) => 
      assertListInt(te, "xs") }
    val scriptMD = script0+"def md(ys: List[Int]): List[Int] = map(double, ys)"
    tcpss(scriptMD) match{ case Ok(te) => 
      assert(te("md") == FunctionType(
        List(), List(ListType(IntType)), ListType(IntType)) )
    }
    // ==================== applyToThree
    tcpss(script0+"val y = applyToThree(double)") match{ case Ok(te) => 
      assert(te("y") == IntType) }
    tcpss( script0+"val y = applyToThree(id)") match{ case Ok(te) =>
      assert(te("y") == IntType)}
    // ===== applyX (returning Int)
    tcpss(script0+"val y = applyX(three, true)") match{ case Ok(te) => 
      assert(te("y") == IntType)
    }
  }

  // =====

  def higherOrderTests2() = {
    println("=higherOrderTests2=")
    // Now apply expecting Eq type. 
    val applyES = "def applyE[A <: Eq](f: A => Int, x: A): Int = f(x)\n"
    val threeES = "def threeE[A <: Eq](x: A): Int = 3\n"
    // Following works, because BoolType unifies against TypeVar with
    // EqTypeConstraint.
    tcpss(applyES+threeS+"val y = applyE(three, true)") match{ case Ok(te) => 
      assert(te("applyE") == FunctionType(
        List(("A",EqTypeConstraint)),
        List(FunctionType(List(), List(TypeParam("A")), IntType),
          TypeParam("A")),
        IntType) )
      assert(te("y") == IntType)
    }

    // Following fails because id does not have an equality type.  Found in
    // Unification.updateEnvToSatisfy.
    assertFail(tcpss(threeES+idS+"val x = threeE(id)"))

    // This works.  Unification.updateEnvToSatisfy finds FunctionType(...)
    // satisfies AnyTypeConstraint.
    tcpss(threeS+idS+"val y = three(id)") match{ case Ok(te) => 
      assert(te("y") == IntType) }

    // The following is fine, because BoolType <: Eq
    tcpss(applyES+threeES+"val y = applyE(threeE, true)") match{ case Ok(te) =>
      assert(te("y") == IntType) }

    // Likewise with this.
    tcpss(applyS+threeES+"val y = apply(threeE, true)") match{
      case Ok(te) => assert(te("y") == IntType) }

    // The following fails, because A is not an EqType.  Found in
    // Unification.updateEnvToSatisfy(..., TypeParam(A)(...),
    // EqTypeConstraint, ...).
    assertFail(tcpss(threeES+"def f[A](x: A): Int = threeE(x)"))

    // The following fails, because t => t (type of id) is not an equality
    // type.  Found in Unification.updateEnvToSatisfy(..., FunctionType(...),
    // EqTypeConstraint, ...).
    assertFail(tcpss(applyS+threeES+idS+"val f = apply(threeE, id)"))

    // The following is straightforward.
    tcpss(applyXS+idS+"val y = applyX(id, 4)") match{ case Ok(te) =>
      assert(te("y") == IntType)
    }
    // Following fails because id isn't A => Int
    assertFail(tcpss(applyXS+idS+"val y = applyX(id, true)"))

    // The following fails.  It doesn't make sense when, e.g., A = Boolean.
    // unification fails to unify FloatType with TypeParam(A).
    assertFail(tcpss("def applyToThreeF[A](f: A => A): A = f(3.0)"))
  }

}
