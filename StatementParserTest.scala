package spreadsheet

/** Tests on parsing of Statements. */
object StatementParserTest extends ParserTest0{
  import Parser.parseAll
  import StatementParser.TestHooks.{statement,statements}

  /** Parse st as a statement, and check its extent. */
  private def ps(st: String): Statement = {
    val res = parseAll(statement, st); checkExtent(res.getExtent, st); res
  }

  private def pss(st: String): List[Statement] = parseAll(statements,st)

  /** Tests on value declarartions and cell writes. */
  private def statements1() = {
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

    assert(pss(s"$dir1\n$dir2\n$vDec") == List(dir1R, dir2R, vDecR) )
    assert(pss(s"$vDec;$dir1;$dir2") == List(vDecR, dir1R, dir2R))

    // Test of typing information at end of line
    assert(pss("#E2 = 42: Int\nval x = 3\n") == List(
      Directive(ColumnExp("E"), RowExp(2), TypedExp(IntExp(42), IntType)),
      ValueDeclaration("x", IntExp(3))
    ))


    // println(parseAll(statements, "Cell(In, firstEmpty+1) = \"Total:\"\n"+
    //   "Cell(Out, firstEmpty+1) = sumCol(Out, #0, firstEmpty)"))

    //println(parseWith(statements,"#B1 = 1+#A1"))
  }

  /** Tests on function declarations. */
  private def functions() = {
    assert(ps("def square(n: Int): Int = n*n") ==
      FunctionDeclaration("square", List(), List(("n",IntType)), IntType,
        BinOp(NameExp("n"), "*", NameExp("n")) ))
    // Test with a newline in a strange place
    assert(ps("def square\n(n: Int): Int = n*n") ==
      FunctionDeclaration("square", List(), List(("n",IntType)), IntType,
        BinOp(NameExp("n"), "*", NameExp("n")) ))
    assert(ps("def add(x: Int, y: Int) : Int = x+y") == 
      FunctionDeclaration("add", List(), 
        List(("x", IntType), ("y", IntType)), IntType,
        BinOp(NameExp("x"), "+", NameExp("y")) ))
    assert(ps("val c = #B \n") == ValueDeclaration("c", ColumnExp("B")))

    assert(parseAll(TypeParser.typeP, "List[Boolean]") ==
      ListType(BoolType))
    assert(ps("def add[A](x: Int, y: Int) : Int = x+y") == 
      FunctionDeclaration("add", List(("A",AnyTypeConstraint)),
        List(("x",IntType), ("y",IntType)), IntType, 
        BinOp(NameExp("x"), "+", NameExp("y")) ))
    assert(ps("def id[A](x: A) : A = x") == 
      FunctionDeclaration("id", List(("A",AnyTypeConstraint)),
        List(("x",TypeParam("A"))), TypeParam("A"), NameExp("x")) )
    // Test with a newline in a strange place.
    assert(ps("def id\n[A](x: A) : A = x") == 
      FunctionDeclaration("id", List(("A",AnyTypeConstraint)),
        List(("x",TypeParam("A"))), TypeParam("A"), NameExp("x")) )

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

    // Following test previously failed when cellExp consumed the "\n".
    assert(pss("val y = #A3\nval x = 3") == List(
      ValueDeclaration("y", UntypedCellExp(ColumnExp("A"), RowExp(3))),
      ValueDeclaration("x", IntExp(3)) ))
  }

  /** Tests on "for" statements. */
  private def forStatements() = {
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
    assert(ps("for (x<-xs) {}") == ForStatement(
      List(Generator("x", NameExp("xs"))), List()
    ))

    //println(ps("for(x <- [#A3]) #B2 = 0"))
  }

  /** Tests on parsing of statements. */
  def apply() = {
    statements1() // value declarations and cell writes.
    functions() // function declarations.
    forStatements() // for statements 
    println("Statement tests done")
  }


}
