package spreadsheet

import Parser._

object ExpParserTest extends ParserTest0{
  import ExpParser.expr

  /** Test of expression parsers on atomic values. */
  private def expressions1() = {
    assert(p("123") == IntExp(123)); assert(p(" ( -123 ) ") == IntExp(-123))
    assert(pe("123.45") == FloatValue(123.45F))
    assert(pe("-456.12") == FloatValue(-456.12F))
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

    assert(p("Cell(#B,#2)") == UntypedCellExp(ColumnExp("B"), RowExp(2)))
    // Following all now allowed
    //assertParseFail("1+Cell(#A,#2)"); 
    // assertParseFail("Cell(#C,#2)+4"); assertParseFail("#D2+1)")
    // assertParseFail("f(#E3)")
 
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
    assert(pe("3 to 5") == ListValue(List(3,4,5).map(IntValue)))
    assert(pe("3 until 5") == ListValue(List(3,4).map(IntValue)))
    assert(pe("#3 to #5") == ListValue(List(3,4,5).map(RowValue)))
    assert(pe("#3 until #5") == ListValue(List(3,4).map(RowValue)))
    assert(pe("#D to #F") == ListValue(List(3,4,5).map(ColumnValue(_))))
    assert(pe("#D until #F") == ListValue(List(3,4).map(ColumnValue(_))))
    assertFail(pe("3 to head([])")); assertFail(pe("head([]) until 3"))
    // Row and column arithmetic
    assert(pe("#D+3") == pe("#G")); assert(pe("#4+2") == pe("#6"))
    assert(pe("#D-2") == pe("#B")); assert(pe("#7-3") == pe("#4"))
    assertFail(pe("#B-4")); assertFail(pe("#3-4"))

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
    // assert(pe("2+5.7") == FloatValue(7.7F))
    // assert(pe("2.8+5") == FloatValue(7.8F))
    // assert(pe("2.3+5.5") == FloatValue(7.8F))
    // assertApprox(pe("4.3-2"), FloatValue(2.3F))
    // assertApprox(pe("4-2.3"), FloatValue(1.7F))
    // assertApprox(pe("4*2.3"), FloatValue(9.2F))
    // assertApprox(pe("4.3*2"), FloatValue(8.6F))
    // assertApprox(pe("4.3/2"), FloatValue(2.15F))
    // assertApprox(pe("7.0/2"), FloatValue(3.5F))

    // assert(pe("2 <= 4.5") == BoolValue(true))
    // assert(pe("2.5 >= 4") == BoolValue(false))
    // assert(pe("2 == 4.5") == BoolValue(false))
    // assert(pe("4.4 == 4") == BoolValue(false))
    // assert(pe("4.0 == 4") == BoolValue(true))
    // assert(pe("4.0 != 4") == BoolValue(false))
  }

  // Note: various tests have been commented out, because the expressions
  // would fail typechecking, and with the current definition of evaluation
  // would throw an exception.

  // ===== Compound expressions

  /** Tests of parsing blocks, if statements, and list expressions. */
  private def expressions3() = {
    // ===== Blocks
    assert(pe("{ val x = 3; x+17 }") == IntValue(20))
    assert(pe("{ val x = 3 \n x+4 }") == IntValue(7))
    assert(pe("{ 4*5 }") == IntValue(20))

    // ===== if statements
    assert(pe("if(2+2 == 4) 3 else 4+2") == IntValue(3))
    assert(pe("if(2+2 == 5) 3 else 4+2") == IntValue(6))
    assert(pe("if(2+2 != 5) 3 else 4+2") == IntValue(3))
    assert(pe("7 * (if(2+2 == 4) 3 else 4+2)") == IntValue(21))
    assert(pe("7 * (if(2+2 == 5) 3 else 4+2)") == IntValue(42))
    assertFail(pe("if(2/0 == 4) 3 else 4"))
    //println(p("if(r == end) 0 else Cell(c,r):Int + sum(c, r+1, end)"))

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
    //println(p("2 < 3 : Bool"))
    //println(p("42: Int\n"))
  }


  /** Tests of expression parsers. */
  def apply() = {
    expressions1() // atomic values
    cellExpressionTests()
    expressions2() // binary operators
    expressions3() // blocks, if statements, list expressions
    typedExpressions()
    println("Expression tests done")
  }

}
