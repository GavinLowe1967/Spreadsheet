package spreadsheet

import Parser._

object ExpParserTest extends ParserTest0{
  //import ExpParser.expr

  /** Test of expression parsers on atomic values. */
  private def expressions1() = {
    assert(p("123") == IntExp(123))
    assert(p(" ( -123 ) ") == IntExp(-123))
    assert(p("foo") == NameExp("foo")); assert(p(" ( foo ) ") == NameExp("foo"))
    assert(p("truely") == NameExp("truely"))

    // Strings
    /* Parse `st` surrounded by quotation marks. */
    def pw(st: String) = p("\""+st+"\"")
    assert(pw("Hello") == StringExp("Hello"))
    val eQ = "\\\"" // String representing an escaped quotation, `\"'
    // The following should fail
    assertParseFail("\"Hello") // expected """
    // println(parseWith(expr, "\"Hello"))
    assert(pw(s"${eQ}Hello${eQ}") == StringExp("\"Hello\""))  // \"Hello\"
    assert(pw("Hello\\nWorld") == StringExp("Hello\nWorld"))
    //println(parseWith(expr, "\"\\Z\""))
    assertParseFail("\"\\Z\"") // Unexpected character Z
    assertParseFail("\"Hello\n\"") // Expected """

    // ===== Rows, columns, cells
    assert(pe("#23") == RowValue(23))
    assert(pe("#Z") == ColumnValue(25)); assert(pe("#AB") == ColumnValue(27))
    // Is the following what we want?? 
    //assert(pe("#Aa") == ColumnValue(0))
    Failure.reset 
    assert(expr("#Aa").asInstanceOf[Success[Exp]].result == ColumnExp("A"))
    Failure.reset
    assert(expr("#a").isInstanceOf[Failure])
  }

  // ===== Cells

  /** Tests on cell expressions. */
  private def cellExpressionTests() = {
    assert(p("Cell(#HW, #23): Int") == 
      CellExp(ColumnExp("HW"), RowExp(23), IntType))
    assert(p("#HW23: Int") == 
      CellExp(ColumnExp("HW"), RowExp(23), IntType))

    assert(p("Cell(#B,#2)") == UntypedCellExp(ColumnExp("B"), RowExp(2)))
    assert(p("#B2") == UntypedCellExp(ColumnExp("B"), RowExp(2)))
    // Following all not now allowed
    //assertParseFail("1+Cell(#A,#2)"); 
    // assertParseFail("Cell(#C,#2)+4"); assertParseFail("#D2+1)")
    // assertParseFail("f(#E3)")
    assertParseFail("Cell(3)")
 
    val matchExp = 
      "#A3 match{ case n: Int => 3; case x:Float=>4 \n "+
        "case _: String => 5; case _ : Boolean => 6 ; \n "+
        "case Empty => 7 }"
    assert(p(matchExp) == CellMatchExp(
      ColumnExp("A"), RowExp(3), List(
        MatchBranch(TypedPattern(Some("n"), IntType), IntExp(3)),
        MatchBranch(TypedPattern(Some("x"), FloatType), IntExp(4)),
        MatchBranch(TypedPattern(None, StringType), IntExp(5)),
        MatchBranch(TypedPattern(None, BoolType), IntExp(6)),
        MatchBranch(EmptyPattern, IntExp(7))
      ) 
    ) )

    val e1 = "Cell(c, r) match{ case Empty => 7; case n: Int => 0; case _ => 3 }"
    assert(p(e1) == CellMatchExp(
      NameExp("c"), NameExp("r"),
      List(MatchBranch(EmptyPattern, IntExp(7)),
        MatchBranch(TypedPattern(Some("n"),IntType), IntExp(0)),
        MatchBranch(Wildcard, IntExp(3))
      )))
    assertParseFail("#A3 match{ }")
  }

  // ===== Binary Ops

  /** Tests of parsing expressions using a binary operator. */
  private def expressions2() = {
    assert(p("2+3") == BinOp(IntExp(2), "+", IntExp(3)))
    assert(p("2+-3") == BinOp(IntExp(2), "+", IntExp(-3)))
    assert(p("2+3-4") == BinOp(BinOp(IntExp(2), "+", IntExp(3)), "-", IntExp(4)))

    assert(p("3 to 5") == BinOp(IntExp(3), "to", IntExp(5)))
    assert(p("threetofive") == NameExp("threetofive"))
    assert(p("#A until #C") == BinOp(ColumnExp("A"), "until", ColumnExp("C")))
    // Following won't typecheck
    assertFail(pe("head([]) until 3"))
    assert(p("3%2") == BinOp(IntExp(3), "%", IntExp(2)))
  }

  // Note: various tests have been commented out, because the expressions
  // would fail typechecking, and with the current definition of evaluation
  // would throw an exception.

  // ===== Compound expressions

  /** Tests of parsing blocks, if statements, and list expressions. */
  private def expressions3() = {
    assert(p("(1,2,3)") == TupleLiteral(List(IntExp(1),IntExp(2),IntExp(3))))
    assert(p(" ( 1, 2.5 ) ") == TupleLiteral(List(IntExp(1),FloatExp(2.5F))))
    assert(p("(1)") == IntExp(1))
  }

  /** Tests for explicitly typed expressions. */
  private def typedExpressions() = {
    assert(p("x : Int") == TypedExp(NameExp("x"), IntType))
    assert(p("#E5: Int") == CellExp(ColumnExp("E"), RowExp(5), IntType))
    assert(p("#E5: Int: Int") == 
      TypedExp(CellExp(ColumnExp("E"), RowExp(5), IntType), IntType) )
    assert(p("true:Boolean") == TypedExp(BoolExp(true), BoolType))
    assert(p("(2 < 3) : Boolean") ==
      TypedExp(BinOp(IntExp(2), "<", IntExp(3)), BoolType) )
    assert(p("2 < 3 : Boolean") == 
      BinOp(IntExp(2), "<", TypedExp(IntExp(3), BoolType)))
  }

  /** Tests for list comprehensions. */
  private def listComprehensions() = {
    assert(p("[x+2 | x <- xs]") == ListComprehension(
      BinOp(NameExp("x"), "+", IntExp(2)), List(Generator("x", NameExp("xs"))) 
    ) )
    assert(p("[x+2 | x <- xs, x > 3]") == ListComprehension(
      BinOp(NameExp("x"), "+", IntExp(2)), 
      List(Generator("x", NameExp("xs")), 
        Filter(BinOp(NameExp("x"), ">", IntExp(3))) )
    ) )

    assert(p("[(Cell(c,#6):Int) | c <- [#A, #D] ]") == ListComprehension(
      CellExp(NameExp("c"), RowExp(6), IntType),
      List(Generator("c", ListLiteral(List(ColumnExp("A"), ColumnExp("D")))))
    ))
  }

  /** Tests on function applications, mostly without parentheses. */
  private def functionApps() = {
    assert(p("f 3") == FunctionApp(NameExp("f"), List(IntExp(3))))
    assert(p("f 3 + g x") == 
      BinOp(FunctionApp(NameExp("f"), List(IntExp(3))), "+",
        FunctionApp(NameExp("g"), List(NameExp("x"))) ))
    assert(p("f \"Hello\"") == 
      FunctionApp(NameExp("f"), List(StringExp("Hello"))))
    assert(p("f true") == FunctionApp(NameExp("f"), List(BoolExp(true))))
    assert(p("f #A3") == 
      FunctionApp(NameExp("f"), List(UntypedCellExp(ColumnExp("A"), RowExp(3)))))
    assert(p("f #A") == FunctionApp(NameExp("f"), List(ColumnExp("A"))))
    assert(p("f [3]") == 
      FunctionApp(NameExp("f"), List(ListLiteral(List(IntExp(3))))))
    assert(p("f {val x = 3; x}") ==
      FunctionApp(NameExp("f"), 
        List(BlockExp(List(ValueDeclaration("x",IntExp(3))), NameExp("x")))))
  }

  /** Tests of expression parsers. */
  def apply() = {
    expressions1() // atomic values
    cellExpressionTests()
    expressions2() // binary operators
    expressions3() // blocks, if statements, list expressions
    typedExpressions() // explicitly typed expressions
    listComprehensions() // list comprehensions
    functionApps() // function applications
    println("Expression tests done")
  }

}
