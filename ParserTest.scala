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
    val e = parseAll(expr, st); println(e)
    println("\""+e.getExtent.asString+"\""); checkExtent(e.getExtent, st)
  }

  val env = new Environment(null, null)

  // Parse and evaluate st, and check the extent
  def pe(st: String) = {
    val v = p(st).eval(env)
    if(v.source == null) println(s"pe: $st -> $v")
    else checkExtent(v.source.asInstanceOf[Extent], st)
    v
  }
  
  // Parse and evaluate st; print result and source
  def pep(st: String) = { val v = pe(st); println(v); println(v.source) }

  /** Tests of expression parsers. */
  def expressions = {
    assert(p("123") == IntExp(123)); assert(p(" ( -123 ) ") == IntExp(-123))
    assert(p("foo") == NameExp("foo")); assert(p(" ( foo ) ") == NameExp("foo"))
    assert(pe("foo").isInstanceOf[ErrorValue])

    assert(p("2+3") == BinOp(IntExp(2), "+", IntExp(3)))
    assert(p("2+-3") == BinOp(IntExp(2), "+", IntExp(-3)))
    assert(p("2+3-4") == BinOp(BinOp(IntExp(2), "+", IntExp(3)), "-", IntExp(4)))

    assert(pe("2+3*4") == IntValue(14)); assert(pe("2*3+4") == IntValue(10))
    assert(pe("(2+3)") == IntValue(5))
    assert(pe("(2+3)*4 == 6") == BoolValue(false))
    assert(pe("(1+4)*4 == 60/3") == BoolValue(true))
    assert(pe("(2+3)*4 != 6") == BoolValue(true))
    assert(pe("(2+3)*4 != 60/3") == BoolValue(false))
    assert(pe("(2+3)*4 > 6") == BoolValue(true))
    assert(pe("(2+3)*4 <= 6 || 6*7 == 42") == BoolValue(true))
    assert(pe("(2+3)*4 <= 6 && 6*7 == 42") == BoolValue(false))
    assert(pe("3 == true").isInstanceOf[TypeError])

    // ===== Rows, columns, cells
    assert(pe("#23") == RowValue(23))
    assert(pe("#Z") == ColumnValue(25)); assert(pe("#AB") == ColumnValue(27))
    // Is the following what we want?? 
    assert(expr("#Aa").asInstanceOf[Success[Exp]].result == ColumnExp("A"))
    assert(expr("#a").isInstanceOf[Failure])
    assert(p("Cell(#HW, #23)") == CellExp(ColumnExp("HW"), RowExp(23)))

    // ===== Blocks
    assert(pe("{ val x = 3; x+1 }") == IntValue(4))
    assert(pe("{ val x = 3\n x+1 }") == IntValue(4))
    assert(pe("{ val x = three; x + 1 }").isInstanceOf[EvalError])
    assert(pe("{ val x = 3; x + four }").isInstanceOf[EvalError])
    assert(pe("{ 4*5 }") == IntValue(20))

    // ===== Functions
    assert(pe("f(2+4,3)").isInstanceOf[EvalError])
    assert(pe("3+1/0").isInstanceOf[EvalError])

    // ===== if statements
    assert(pe("if(2+2 == 4) 3 else 4+2") == IntValue(3))
    assert(pe("if(2+2 == 5) 3 else 4+2") == IntValue(6))
    assert(pe("if(2+2 != 5) 3 else 4+2") == IntValue(3))
    assert(pe("7 * (if(2+2 == 4) 3 else 4+2)") == IntValue(21))
    assert(pe("7 * (if(2+2 == 5) 3 else 4+2)") == IntValue(42))
    assert(pe("if(2/0 == 4) 3 else 4").isInstanceOf[EvalError])
    assert(pe("if(3+4) 7 else 12").isInstanceOf[TypeError])

    // ===== List expressions
    // println(pe("[4/4, 2+0, 6-3]").forError)
    assert(pe("[]") == ListValue(AnyType, List()))
    assert(pe("[4/4, 2+0, 6-3]") == 
      ListValue(IntType, List(IntValue(1), IntValue(2), IntValue(3))))
    assert(pe("[4/2, 3/0]").isInstanceOf[EvalError])
    assert(pe("[true, 4]").isInstanceOf[TypeError])

    assert(pe("head([1,2,3])") == IntValue(1))
    assert(pe("head([])").isInstanceOf[EvalError])
    assert(pe("head(3)").isInstanceOf[TypeError])

    assert(pe("tail([1,2,3])") == 
      ListValue(IntType, List(IntValue(2), IntValue(3))))
    assert(pe("tail([])").isInstanceOf[EvalError])
    assert(pe("tail(3)").isInstanceOf[TypeError])

    assert(pe("[1,2] == 3").isInstanceOf[TypeError])
    assert(pe("[1,2] == [3,4]") == BoolValue(false))
    assert(pe("[1,2] != tail([3,1,2])") == BoolValue(false))
    assert(pe("[1,2] == tail([3,1,2])") == BoolValue(true))
    assert(pe("tail([1]) == []") == BoolValue(true))
    assert(pe("[] == tail([1])") == BoolValue(true))
    assert(pe("tail([1]) == tail([false])").isInstanceOf[TypeError])

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

    def cellExp(c: String, r: Int) = CellExp(ColumnExp(c), RowExp(r))
    val dir1 = "Cell(#A, #3) = Cell(#A, #1) + Cell(#A, #2)"
    val dir1R = 
      Directive(cellExp("A",3), BinOp(cellExp("A",1), "+", cellExp("A",2)))
    assert(ps(dir1) == dir1R)

    val dir2 = "#B3 = #B1 + #B2 - three" 
    val dir2R = Directive(
      cellExp("B",3),
      BinOp( BinOp(cellExp("B",1), "+", cellExp("B",2)),  "-", NameExp("three") )
    )
    assert(ps(dir2) == dir2R)
    // println(parseAll(directive, dir2))

    assert(parseAll(statements, s"$dir1\n$dir2\n$vDec") == 
      List(dir1R, dir2R, vDecR) )
    // println(statements(s"$dir1\n$dir2\n$vDec"))
    // println(parseAll(statements, s"$dir1\n$dir2\n$vDec"))
    assert(parseAll(statements, s"$vDec;$dir1;$dir2") ==
      List(vDecR, dir1R, dir2R))

    // println(parseAll(params, "n : Int"))
    // println(parseAll(statement, "def square(n: Int) = n*n"))
    assert(ps("def square(n: Int): Int = n*n") ==
      FunctionDeclaration("square", List(("n",IntType)), IntType,
        BinOp(NameExp("n"), "*", NameExp("n")) ))
    //println(parseAll(statement, "def add(x: Int, y: Int) = x*y"))
    assert(ps("def add(x: Int, y: Int) : Int = x+y") == 
      FunctionDeclaration("add", List(("x", IntType), ("y", IntType)), IntType,
        BinOp(NameExp("x"), "+", NameExp("y")) ))

    // println(parseWith(statement, "def add(x: Int, y: Int) = x+y"))

    // println(ps("def square(n) = n*n"))

    // println(parseAll(statements, s"$vDec\n$dir2\n$dir1\n"))
    println("Statement tests done")
  }

  def main(args: Array[String]) = {
    expressions; testStatements;  println("Done")
  }






}
