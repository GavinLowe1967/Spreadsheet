package spreadsheet

import TypeT._
import TypeCheckerTest0._

object TypeCheckerTest4{
  /** Something similar to the standard prelude. */
  val script0 = (
    "def foldr[A,B](f: A => B => B)(e: B)(xs: List[A]): B = "+
      "  if(isEmpty xs) e else f(head xs)(foldr f e (tail xs))\n"+
      "def foldl[A,B](f: B => A => B)(e: B)(xs: List[A]): B = " +
      "  if(isEmpty xs ) e else foldl f (f e (head xs)) (tail xs) \n"+
      "def cons[A](x: A)(xs: List[A]) = x::xs \n" +
      "def length[A](xs: List[A]) = { def f(x:A)(n:Int) = n+1; foldr f 0 xs }\n"+
      "def append[A](xs: List[A])(ys: List[A]) = foldr cons ys xs\n"+
      "def concat[A](xs: List[List[A]]) = foldr append[]xs \n" +
      "def after[A,B,C](f: B => C)(g: A => B)(x: A) = f(g x) \n" +
      "def map[A,B](f: A => B)(xs: List[A]): List[B] = " +
      "  if(isEmpty xs) [] else f(head xs) :: map f (tail xs) \n" +
      "def reverse[A](xs: List[A]) = {"+
      "  def f(ys: List[A])(x: A) = x::ys; foldl f [] xs }\n"  + 
      "def filter[A](p: A => Boolean)(xs: List[A]): List[A] = " +
      "  if(isEmpty xs) [] "+
      "else { val x = head xs; " +
      "       if(p x) x :: filter p(tail xs) else filter p (tail xs) }\n"
  )

  /** Definitions using functions from script0. */
  val script1 = (
    "val sum = { def p(x:Int)(y:Int) = x+y; foldr p 0 }\n" +
      "val sum1 = { def p(x:Int)(y:Int) = x+y; foldl p 0 }\n" +
      "val sum2 = { def p(x:Float)(y:Float) = x+y; foldr p 0.0 }\n" +
      "val sum3 = { def p(x:Float)(y:Float) = x+y; foldl p 0.0 }\n" +
      "val lengthp = { def f[A](x:A)(n:Int) = n+1; foldr f 0 } \n" +
      "val xs = concat [[3]]; val ys = concat[[2.3]] \n" +
      "val concatp = foldr append [] \n" +
      "val xsp = concatp [[3]] \n" +
      "def map1[A,B](f: A => B) = { "+
      "  def g(x: A)(ys: List[B]) = cons (f x) ys; foldr g [] }\n" +
      "def map2[A,B](f: A => B) = foldr(after cons f) []\n"+
      "def mapLength[A](xs: List[List[A]]) = map length xs \n" +
      "val mapLength1 = map length \n" + 
      "val ls = mapLength1 [[3]] ; val ls2 = mapLength1 [[3.5]]\n" +
      "val filterP = { def pos(x: Int) = x > 0; filter pos }"
  )

  val script = script0 + "\n" + script1

  def preludeTests() = {
    println("===preludeTests===")
    tcpss(script) match{ 
      case Ok(te) => okTests(te); case FailureR(err) => println(err)
    }
  }

  /** Tests done on the environment produced by typechecking script. */
  def okTests(te: TypeEnv) = {
    // foldr: [A,B] (A => B => B) => B => List[A] => B
    assert(te("foldr") == FunctionType(
      List(("A",AnyTypeConstraint), ("B",AnyTypeConstraint)),
      List(FunctionType(List(), List(TypeParam("A")),
        FunctionType(List(), List(TypeParam("B")), TypeParam("B")))),
      FunctionType(List(), List(TypeParam("B")),
        FunctionType(List(), List(ListType(TypeParam("A"))), TypeParam("B"))
      )))

    // foldl : [A,B] (B => A => B) => B => List[A] => B
    assert(te("foldl") == FunctionType(
      List(("A",AnyTypeConstraint), ("B",AnyTypeConstraint)),
      List(FunctionType(List(), List(TypeParam("B")),
        FunctionType(List(), List(TypeParam("A")), TypeParam("B")) )),
      FunctionType(List(), List(TypeParam("B")),
        FunctionType(List(), List(ListType(TypeParam("A"))), TypeParam("B")) )))

    // cons :: [A] A => List[A] => List[A]
    assert(te("cons") == FunctionType(
      List(("A",AnyTypeConstraint)),
      List(TypeParam("A")),
      FunctionType( List(), 
        List(ListType(TypeParam("A"))), ListType(TypeParam("A")) )))

    assert(te("sum") == FunctionType(List(), List(ListType(IntType)), IntType))
    assert(te("sum1") == FunctionType(List(), List(ListType(IntType)), IntType))
    assert(te("sum2") == 
      FunctionType(List(), List(ListType(FloatType)), FloatType))
    assert(te("sum3") == 
      FunctionType(List(), List(ListType(FloatType)), FloatType))

    // length: [A] List[A] => Int
    assert(te("length") == FunctionType(
      List(("A",AnyTypeConstraint)), List(ListType(TypeParam("A"))), IntType))
    // lengthp: [A1] List[A1] => Int
    te("lengthp") match{
      case FunctionType(
        List((a,AnyTypeConstraint)), List(ListType(TypeParam(a1))), IntType
      ) => assert(a == a1)
    }

    // append: [A] List[A] => List[A] => List[A]
    assert(te("append") == FunctionType(
      List(("A",AnyTypeConstraint)), List(ListType(TypeParam("A"))),
      FunctionType(List(), List(ListType(TypeParam("A"))),
        ListType(TypeParam("A")) ) ))

    // concat :: [A] List[List[A]] => List[A]
    assert(te("concat") == FunctionType(
      List(("A",AnyTypeConstraint)),
      List(ListType(ListType(TypeParam("A")))), ListType(TypeParam("A"))))
    assert(te("xs") == ListType(IntType) && te("ys") == ListType(FloatType))
    // concatp has type in terms of a type variable
    te("concatp") match{
      case FunctionType(
        List(), List(ListType(ListType(TypeVar(t)))), ListType(TypeVar(t1))
      ) => assert(t1 == t)
    }
    assert(te("xsp") == ListType(IntType))
    // Note: the "[]" in the definition of concatp has a specific type,
    // which is constrained to be IntType by the definition of xsp; so we
    // can't now apply it to [[2.3]], say.

    // map: [A,B] (A => B) => List[A] => List[B]
    val mapType = FunctionType(
      List(("A",AnyTypeConstraint), ("B",AnyTypeConstraint)),
      List( FunctionType(List(), List(TypeParam("A")), TypeParam("B")) ),
      FunctionType( List(), 
        List(ListType(TypeParam("A"))), ListType(TypeParam("B")) ))
    assert(te("map") == mapType); assert(te("map1") == mapType)
    assert(te("map2") == mapType)

    assert(te("mapLength") == FunctionType(
      List(("A",AnyTypeConstraint)),
      List(ListType(ListType(TypeParam("A")))), ListType(IntType)) )
    te("mapLength1") match{
      case FunctionType(
        List((a,AnyTypeConstraint)), 
        List(ListType(ListType(TypeParam(a1)))), ListType(IntType)
      ) => assert(a == a1)
    }
    assert(te("ls") == ListType(IntType)); assert(te("ls2") == ListType(IntType))

    assert(te("reverse") == FunctionType(
      List(("A",AnyTypeConstraint)), 
      List(ListType(TypeParam("A"))), ListType(TypeParam("A")) ))

    // filter: [A] (A => Boolean) => List[A] => List[A]
    assert(te("filter") == FunctionType(
      List(("A",AnyTypeConstraint)),
      List(FunctionType(List(), List(TypeParam("A")), BoolType)),
      FunctionType(List(), List(ListType(TypeParam("A"))), 
        ListType(TypeParam("A")) )))

    assert(te("filterP") == 
      FunctionType(List(), List(ListType(IntType)), ListType(IntType)) )
  }

  /** Tests where return type of function omitted. */
  def rtTests() = {
    println("===rtTests===")
    tcpss("def succ(x: Int) = x+1") match{ case Ok(te) =>
      assert(te("succ") == FunctionType(List(),List(IntType),IntType))
    }
    tcpss("def add(x: Int, y: Int) = x+y") match{ case Ok(te) =>
      assert(te("add") == FunctionType(List(), List(IntType, IntType), IntType))
    }
    // Now with overloading.
    val script1 = 
      "def add(x: Int, y: Int) = x+y; def add(x: Float, y: Float) = x+y"
    tcpss(script1) match{ case Ok(te) =>
      assert(te.get("add") == Some(List(
        FunctionType(List(), List(IntType, IntType), IntType), 
        FunctionType(List(), List(FloatType, FloatType), FloatType)
      )))
      // Note: the order matches the order of declaration in the script, at
      // least with the current implementation.
    }
    // Now with currying.
    tcpss("def add(x: Int)(y: Int) = x+y") match{ case Ok(te) => 
      assert(te("add") == FunctionType(
        List(), List(IntType),
        FunctionType(List(), List(IntType), IntType) ))
    }
    val script2 = 
      "def add(x: Int)(y: Int) = x+y; def add(x: Float)(y: Float) = x+y"
    tcpss(script2) match{ case Ok(te) =>
      assert(te.get("add") == Some(List(
        FunctionType(List(), List(IntType),
          FunctionType(List(), List(IntType), IntType)),
        FunctionType(List(), List(FloatType),
          FunctionType(List(), List(FloatType), FloatType) ))))
    }
    // Bad overloading.
    val script3 = "def add(x: Int, y: Int) = x+y; def add(x: Int, y: Int) = x+y"
    // "Function add has multiple definitions with parameters of type Int, Int" 
    assertFail(tcpss(script3))

    // One function calling an earlier one.
    val script4 = "def f(x: Int) = x+1; def g(y: Int, z: Int) = f(y+z)"
    tcpss(script4) match{ case Ok(te) =>
      assert(te("f") == FunctionType(List(), List(IntType), IntType))
      assert(te("g") == FunctionType(List(), List(IntType, IntType), IntType))
    }
    val script5 = "def f[A](x: A) = x; def g(y: Int, z: Int) = f(y+z)"
    tcpss(script5) match{ case Ok(te) => 
      assert(te("f") == FunctionType(
        List(("A", AnyTypeConstraint)), List(TypeParam("A")), TypeParam("A")))
      assert(te("g") == FunctionType(List(), List(IntType, IntType), IntType))
    }
    // Forward reference to function with type.
    val script6 = "def g(y: Int, z: Int) = f(y+z); def f(x: Int): Int = x+1"
    tcpss(script6) match{ case Ok(te) =>
      assert(te("f") == FunctionType(List(), List(IntType), IntType))
      assert(te("g") == FunctionType(List(), List(IntType, IntType), IntType))
    }
    // Forward reference to function without type. 
    val script7 = "def g(y: Int) = f(y+2); def f(x: Int) = x+1"
    // "Function without explicit return type applied recursively, or before
    // its definition, at line 1 in f(y+2) in ..."
    assertFail(tcpss(script7))
    // println(tcpss(script7))

    // Reference to overloaded function.
    val script8 = 
      "def f(x: Int) = x; def f(x: Float) = x; def g(x: Float) = f(x)"
    tcpss(script8) match{ case Ok(te) =>
      assert(te("g") == FunctionType(List(), List(FloatType), FloatType))
    }
    val script9 =
      "def g(x: Float) = f(x); def f(x: Int) = x; def f(x: Float): Float = x"
    tcpss(script9) match{ case Ok(te) =>
      assert(te("g") == FunctionType(List(), List(FloatType), FloatType))
    }
    val script10 =
      "def g(x: Float) = f(x); def f(x: Int) = x; def f(x: Float) = x"
    assertFail(tcpss(script10))
    //println(tcpss(script10))

    val script11 = "def fact(n: Int) = if(n==0) 1 else n*fact(n-1)"
    assertFail(tcpss(script11))

    // Reference to curried function
    val script12 = "val add3 = add(3); def add(x: Int)(y: Int) = x+y"
    assertFail(tcpss(script12))

    val script12a = "val add3 = add(3); def add(x: Int)(y: Int): Int = x+y"
    tcpss(script12a) match{ case Ok(te) =>
      assert(te("add3") == FunctionType(List(), List(IntType), IntType))
    }

    val script13 = 
      "val add3 = add(3); "+
      "def add(x: Int)(y: Int) = x+y; def add(x: Float)(y: Float) = x+y"
    assertFail(tcpss(script13))
    val script13a = 
      "val add3 = add(3); "+
      "def add(x: Int)(y: Int): Int = x+y; def add(x: Float)(y: Float) = x+y"
    tcpss(script13a) match{ case Ok(te) =>
      assert(te("add3") == FunctionType(List(), List(IntType), IntType))
    }
  }

  /** Tests involving tuples. */
  def tupleTests() = {
    println("===tupleTests===")
    tcpss("def pair(x: Int): (Int,Int) = (x,x)") match{ case Ok(te) =>
      assert(te("pair") == FunctionType(
        List(), List(IntType), TupleType(List(IntType, IntType))))
    }
    tcpss("def pair[A,B](x: A, y: B): (A,B) = (x,y)") match{ case Ok(te) =>
      assert(te("pair") == FunctionType(
        List(("A", AnyTypeConstraint), ("B", AnyTypeConstraint)),
        List(TypeParam("A"), TypeParam("B")),
        TupleType(List(TypeParam("A"), TypeParam("B"))) ))
    }
    // tcpss("val pair = (1,2.5); val x = get1From2(pair)") match{ case Ok(te) =>
    //   assert(te("pair") == TupleType(List(IntType,FloatType)))
    //   assert(te("x") == IntType)
    // }
    tcpss("val pair = (1,2.5); val x = get1(pair)") match{ case Ok(te) =>
      assert(te("pair") == TupleType(List(IntType,FloatType)))
      assert(te("x") == IntType)
    }
    // Expected (t379,t380,t381), found (Int,Float)
    // assertFail(tcpss("val pair = (1,2.5); val x = get1From3 pair"))
    assertFail(tcpss("val pair = (1,2.5); val x = get3 pair"))
    tcpss("def pair[A,B](x: A, y: B) = (x,y); val p = pair (2,3.5)") match{ 
      case Ok(te) => assert(te("p") == TupleType(List(IntType,FloatType)))
    }
    tcpss("def fst[A,B](p: (A,B)) = get1 p; val x = fst((2.3, 5))") match{
      case Ok(te) => assert(te("x") == FloatType) 
    }
    //println(tcpss("def id[A](x: A) = x; val y = id 3"))

  }

}
