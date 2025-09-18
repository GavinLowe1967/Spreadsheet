package spreadsheet

import Parser._

/** Tester for parsers. */
object ParserTest{
  import ExpParser.expr

  def isWhite(c: Char) = c == ' ' || c == '\t' || c == '\n'
   
  // Remove leading and training whilespace from st
  def trim(st: String) =
    st.dropWhile(isWhite).reverse.dropWhile(isWhite).reverse
    
  // Check that ext matches st
  def checkExtent(ext: Extent, st: String) = {
    assert(ext != null, s"Null extent with $st")
    val st1 = trim(st); val st2 = ext.asString
    assert(st2 == st1, s"\"$st2\" != \"$st1\"")
  }

  // Parse as expression
  def p0(st: String): Exp = parseAll(expr, st)
  
  // Parse as expression, and check extent
  def p(st: String): Exp = {
    val e = parseAll(expr, st); checkExtent(e.getExtent, st); e
  }
  
  // Parse and print the extent
  def pp(st: String) = {
    val e = parseAll(expr, st); println(s"$e.")
    println("\""+e.getExtent.asString+"\""); checkExtent(e.getExtent, st)
  }

  val env = new Environment(null, null, 0, 0, null)

  // Parse and evaluate st, and check the extent
  def pe(st: String) = {
    val v = Execution.TestHooks.eval(env, p(st))
    if(v.source == null) println(s"pe: $st -> $v")
    else checkExtent(v.source.asInstanceOf[Extent], st)
    v
  }
  
  // Parse and evaluate st; print result and source
  def pep(st: String) = { val v = pe(st); println(v); println(v.source) }

  var printErrors = false

  /** Check that v is an error, printing the error message if printErrors is
    * set. */
  def assertFail(v: Value) = v match{
    case EvalError(err) => if(printErrors) println(err)
    case _ => sys.error(s"Error expected, $v found")
  }

  /** Assert that v1 and v2 are FloatValues that are approximately equal.  Pesky
    * rounding errors! */   
  def assertApprox(v1: Value, v2: Value) = (v1,v2) match{
    case (FloatValue(f1), FloatValue(f2)) => 
      assert(Math.abs(f1-f2) < 0.00001, s"$f1 $f2")
  }

  // Note: various tests have been commented out, because the expressions
  // would fail typechecking, and with the current definition of evaluation
  // would throw an exception.

  /** Tests of expression parsers. */
  def expressions = {
    assert(p("123") == IntExp(123)); assert(p(" ( -123 ) ") == IntExp(-123))
    assert(pe("123.45") == FloatValue(123.45F))
    assert(pe("-456.12") == FloatValue(-456.12F))
    assert(p("foo") == NameExp("foo")); assert(p(" ( foo ) ") == NameExp("foo"))

    assert(p("2+3") == BinOp(IntExp(2), "+", IntExp(3)))
    assert(p("2+-3") == BinOp(IntExp(2), "+", IntExp(-3)))
    assert(p("2+3-4") == BinOp(BinOp(IntExp(2), "+", IntExp(3)), "-", IntExp(4)))

    assert(p("3 to 5") == BinOp(IntExp(3), "to", IntExp(5)))
    assert(p("threetofive") == NameExp("threetofive"))
    assert(p("#A until #C") == BinOp(ColumnExp("A"), "until", ColumnExp("C")))

    assert(pe("2+3*4") == IntValue(14)); assert(pe("2*3+4") == IntValue(10))
    assert(pe("(2+3)") == IntValue(5))
    assert(pe("(2+3)*4 == 6") == BoolValue(false))
    assert(pe("(1+4)*4 == 60/3") == BoolValue(true))
    assert(pe("(2+3)*4 != 6") == BoolValue(true))
    assert(pe("(2+3)*4 != 60/3") == BoolValue(false))
    assert(pe("(2+3)*4 > 6") == BoolValue(true))
    assert(pe("(2+3)*4 <= 6 || 6*7 == 42") == BoolValue(true))
    assert(pe("(2+3)*4 <= 6 && 6*7 == 42") == BoolValue(false))
    assertFail(pe("3/0+4"))
    assertFail(pe("2+5/0"))

    // Tests mixing floats and ints
    assert(pe("2+5.7") == FloatValue(7.7F))
    assert(pe("2.8+5") == FloatValue(7.8F))
    assert(pe("2.3+5.5") == FloatValue(7.8F))
    println(pe("4.3-2"))
    assertApprox(pe("4.3-2"), FloatValue(2.3F))
    assertApprox(pe("4-2.3"), FloatValue(1.7F))
    assertApprox(pe("4*2.3"), FloatValue(9.2F))
    assertApprox(pe("4.3*2"), FloatValue(8.6F))
    assertApprox(pe("4.3/2"), FloatValue(2.15F))
    assertApprox(pe("7.0/2"), FloatValue(3.5F))

    assert(pe("2 <= 4.5") == BoolValue(true))
    assert(pe("2.5 >= 4") == BoolValue(false))

    assert(pe("2 == 4.5") == BoolValue(false))
    assert(pe("4.4 == 4") == BoolValue(false))
    assert(pe("4.0 == 4") == BoolValue(true))
    assert(pe("4.0 != 4") == BoolValue(false))

    // ===== Rows, columns, cells
    assert(pe("#23") == RowValue(23))
    assert(pe("#Z") == ColumnValue(25)); assert(pe("#AB") == ColumnValue(27))
    // Is the following what we want?? 
    assert(expr("#Aa").asInstanceOf[Success[Exp]].result == ColumnExp("A"))
    assert(expr("#a").isInstanceOf[Failure])
    assert(p("Cell(#HW, #23): Int") == 
      CellExp(ColumnExp("HW"), RowExp(23), IntType))

    // ===== Blocks
    assert(pe("{ val x = 3; x+1 }") == IntValue(4))
    assert(pe("{ val x = 3\n x+1 }") == IntValue(4))
    assert(pe("{ 4*5 }") == IntValue(20))

    // ===== Functions

    // ===== if statements
    assert(pe("if(2+2 == 4) 3 else 4+2") == IntValue(3))
    assert(pe("if(2+2 == 5) 3 else 4+2") == IntValue(6))
    assert(pe("if(2+2 != 5) 3 else 4+2") == IntValue(3))
    assert(pe("7 * (if(2+2 == 4) 3 else 4+2)") == IntValue(21))
    assert(pe("7 * (if(2+2 == 5) 3 else 4+2)") == IntValue(42))
    assertFail(pe("if(2/0 == 4) 3 else 4"))

    // ===== List expressions
    assert(pe("[]") == ListValue(/*AnyType,*/ List()))
    assert(pe("[4/4, 2+0, 6-3]") == 
      ListValue(IntValue(1), IntValue(2), IntValue(3)))
    assertFail(pe("[4/2, 3/0]"))

    assert(pe("head([1,2,3])") == IntValue(1))
    assertFail(pe("head([])"))

    assert(pe("tail([1,2,3])") == ListValue(IntValue(2), IntValue(3)))
    assertFail(pe("tail([])"))

    assert(pe("[1,2] == [3,4]") == BoolValue(false))
    assert(pe("1 :: 2 :: []") == ListValue(IntValue(1), IntValue(2)))
    assert(pe("[1,2] != tail([3,1,2])") == BoolValue(false))
    assert(pe("[1,2] == tail([3,1,2])") == BoolValue(true))
    assert(pe("tail([1]) == []") == BoolValue(true))
    assert(pe("[] == tail([1])") == BoolValue(true))
    // assert(pe("tail([1]) == tail([false])").isInstanceOf[TypeError])

    println("Expression tests done")
  }

  import StatementParser.{statement,statements}

  /** Parse st as a statement, and check its extent. */
  def ps(st: String): Statement = {
    val res = parseAll(statement, st); checkExtent(res.getExtent, st); res
  }
  /** Parse st as a directive, and check its extent. */
  // def pd(st: String): Statement = {
  //   val res = parseAll(directive, st); checkExtent(res.getExtent, st); res
  // }

  def testStatements = {
    val vDec = "val three = 1+2"
    val vDecR = ValueDeclaration("three", BinOp(IntExp(1), "+", IntExp(2)))
    assert(ps(vDec) == vDecR)

    def cellExp(c: String, r: Int, t: CellType) = 
      CellExp(ColumnExp(c), RowExp(r), t)
    val dir1 = "Cell(#A, #3) = Cell(#A, #1): Float + Cell(#A, #2): Int"
    val dir1R = Directive(ColumnExp("A"), RowExp(3), 
      BinOp(cellExp("A",1,FloatType), "+", cellExp("A",2,IntType)))
    assert(ps(dir1) == dir1R)

    val dir2 = "#B3 = #B1: Int + #B2: Int - three" 
    val dir2R = Directive(ColumnExp("B"), RowExp(3),
      BinOp( BinOp(cellExp("B",1,IntType), "+", cellExp("B",2,IntType)),
        "-", NameExp("three") )
    )
    assert(ps(dir2) == dir2R)

    assert(parseAll(statements, s"$dir1\n$dir2\n$vDec") == 
      List(dir1R, dir2R, vDecR) )
    assert(parseAll(statements, s"$vDec;$dir1;$dir2") ==
      List(vDecR, dir1R, dir2R))

    assert(ps("def square(n: Int): Int = n*n") ==
      FunctionDeclaration("square", List(), List(("n",IntType)), IntType,
        BinOp(NameExp("n"), "*", NameExp("n")) ))
    assert(ps("def add(x: Int, y: Int) : Int = x+y") == 
      FunctionDeclaration("add", List(), 
        List(("x", IntType), ("y", IntType)), IntType,
        BinOp(NameExp("x"), "+", NameExp("y")) ))
    assert(ps("val c = #B \n") == ValueDeclaration("c", ColumnExp("B")))

    assert(parseAll(StatementParser.typeP, "List[Boolean]") ==
      ListType(BoolType))
    assert(ps("def add[A](x: Int, y: Int) : Int = x+y") == 
      FunctionDeclaration("add", List(("A",AnyTypeConstraint)),
        List(("x",IntType), ("y",IntType)), IntType, 
        BinOp(NameExp("x"), "+", NameExp("y")) ))
    assert(ps("def id[A](x: A) : A = x") == 
      FunctionDeclaration("id", List(("A",AnyTypeConstraint)),
        List(("x",TypeParam("A"))), TypeParam("A"), NameExp("x")) )

    // "for" statements
    assert(ps("for (if true){ #A1 = 3 }") == ForStatement(
      List(Filter(BoolExp(true))), 
      List(Directive(ColumnExp("A"), RowExp(1), IntExp(3))) ))
    assert(ps("for (x<-xs)  #A1 = x ") == ForStatement(
      List(Generator("x", NameExp("xs"))),
      List(Directive(ColumnExp("A"), RowExp(1), NameExp("x"))) ))
    //println(ps("for (r <- [#A,#B]; if r != #C) Cell(r,3) = 5"))
    ps("for (r <- [#A,#B]; if r != #C){ val f = 5; Cell(r,3) = f }") match{ 
      case ForStatement(bs,sts) =>
        assert(bs.length == 2 && sts.length == 2)
        assert(bs(0) ==
          Generator("r", ListLiteral(List(ColumnExp("A"), ColumnExp("B")))) )
        assert(bs(1) == Filter(BinOp(NameExp("r"), "!=", ColumnExp("C"))))
        assert(sts(0) == ValueDeclaration("f", IntExp(5)))
        assert(sts(1) == Directive(NameExp("r"), IntExp(3), NameExp("f")))
    }

    println("Statement tests done")
  }

  def main(args: Array[String]) = {
    printErrors = true

    expressions; testStatements;  

    assert(ps("def apply[A, B](f: A => B, x: A) : B = f(x)") == 
      FunctionDeclaration("apply", 
        List(("A",AnyTypeConstraint), ("B",AnyTypeConstraint) ),
        List(("f", FunctionType(List(), List(TypeParam("A")), TypeParam("B"))), 
          ("x", TypeParam("A"))),
        TypeParam("B"), 
        FunctionApp(NameExp("f"), List(NameExp("x")) ) ))

    assert(ps("def f[A <: Eq](x: A): Boolean = x == x") ==
      FunctionDeclaration(
        "f", List(("A",EqTypeConstraint)),
        List(("x",TypeParam("A"))), BoolType, 
        BinOp(NameExp("x"), "==", NameExp("x")) ))

    assertFail(pe("{ def f(x: Int): Int = 5/x; f(0) }"))
    assertFail(pe("{ def f(x: Int): Int = 5/x; f(1/0) }"))

    println("Done")
  }






}
