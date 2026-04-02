package spreadsheet

/** Tests involving parsing, type checking and evaluation of expressions,
  * independent of any cell values. */
object EvaluationTest{
  val expr = DeclarationParser.expParser.expr
  val typeCheckAndClose =  DeclarationTypeChecker.etc.typeCheckAndClose _

  /** Parse, type check and evaluate st.  The parsing and type checking are
    * expected to succeed; but evaluation may fail. */
  def eval(st: String): Value = {
    val e = Parser.parseAll(expr, st); val env = TypeEnv()
    typeCheckAndClose(env, e) match{
      case Ok((te,t)) => 
        val env = Environment(0,0); Execution.TestHooks.eval(env,e)
      case FailureR(err) => println(err); null
    }
  }

  /** Assert that v is an ErrorValue. */
  def assertFail(v: Value) = v match{
    case _: ErrorValue => {}; case _ => sys.error(s"Expected error, found $v")
  }

  /** Tests on basic expressions. */
  private def tests1() = {
    // Arithmetic expressions
    assert(eval("123.45") == FloatValue(123.45F))
    assert(eval("-456.12") == FloatValue(-456.12F))
    assert(eval("2+3") == IntValue(5))
    assert(eval("2+3*4") == IntValue(14)); assert(eval("2*3+4") == IntValue(10))
    assert(eval("(2+3)") == IntValue(5))
    assert(eval("12%5") == IntValue(2)); assertFail(eval("3%0"))
    assert(eval("(2+3)*4 == 6") == BoolValue(false))
    assert(eval("(1+4)*4 == 60/3") == BoolValue(true))
    assert(eval("(2+3)*4 != 6") == BoolValue(true))
    assert(eval("(2+3)*4 != 60/3") == BoolValue(false))
    assert(eval("(2+3)*4 > 6") == BoolValue(true))
    assert(eval("(2+3)*4 <= 6 || 6*7 == 42") == BoolValue(true))
    assert(eval("(2+3)*4 <= 6 && 6*7 == 42") == BoolValue(false))
    assertFail(eval("3/0+4")); assertFail(eval("2+5/0"))
    // "to" and "until"
    assert(eval("3 to 5") == ListValue(List(3,4,5).map(IntValue)))
    assert(eval("3 until 5") == ListValue(List(3,4).map(IntValue)))
    assert(eval("#3 to #5") == ListValue(List(3,4,5).map(RowValue)))
    assert(eval("#3 until #5") == ListValue(List(3,4).map(RowValue)))
    assert(eval("#D to #F") == ListValue(List(3,4,5).map(ColumnValue(_))))
    assert(eval("#D until #F") == ListValue(List(3,4).map(ColumnValue(_))))
    assertFail(eval("3 to head([])"))
    // Row and column arithmetic
    assert(eval("#D+3") == eval("#G")); assert(eval("#4+2") == eval("#6"))
    assert(eval("#D-2") == eval("#B")); assert(eval("#7-3") == eval("#4"))
    assertFail(eval("#B-4")); assertFail(eval("#3-4"))
    // Tyeval conversion
    assert(eval("toInt 4.5") == IntValue(4))
    assert(eval("toFloat 3") == FloatValue(3.0F))
    // Tuples
    assert(eval("(2,3.5)") == TupleValue(IntValue(2), FloatValue(3.5F)))
  }

  /** Blocks, if statements, list expressions. */
  private def tests2() = {
    // ===== Blocks
    assert(eval("{ val x = 3; x+17 }") == IntValue(20))
    assert(eval("{ val x = 3 \n x+4 }") == IntValue(7))
    assert(eval("{ 4*5 }") == IntValue(20))
    // ===== if statements
    assert(eval("if(2+2 == 4) 3 else 4+2") == IntValue(3))
    assert(eval("if(2+2 == 5) 3 else 4+2") == IntValue(6))
    assert(eval("if(2+2 != 5) 3 else 4+2") == IntValue(3))
    assert(eval("7 * (if(2+2 == 4) 3 else 4+2)") == IntValue(21))
    assert(eval("7 * (if(2+2 == 5) 3 else 4+2)") == IntValue(42))
    assertFail(eval("if(2/0 == 4) 3 else 4"))
    // ===== List expressions
    assert(eval("[]") == ListValue(/*AnyTyeval,*/ List()))
    assert(eval("[4/4, 2+0, 6-3]") == 
      ListValue(IntValue(1), IntValue(2), IntValue(3)))
    assertFail(eval("[4/2, 3/0]"))
    assert(eval("head([1,2,3])") == IntValue(1))
    assertFail(eval("head([])"))
    assert(eval("tail([1,2,3])") == ListValue(IntValue(2), IntValue(3)))
    assertFail(eval("tail([])"))
    assert(eval("[1,2] == [3,4]") == BoolValue(false))
    assert(eval("1 :: 2 :: []") == ListValue(IntValue(1), IntValue(2)))
    assert(eval("[1,2] != tail([3,1,2])") == BoolValue(false))
    assert(eval("[1,2] == tail([3,1,2])") == BoolValue(true))
    assert(eval("tail([1]) == []") == BoolValue(true))
    assert(eval("[] == tail([1])") == BoolValue(true))

  }


  def main(args: Array[String]) = {
    println("===EvaluationTest===")
    tests1()
    tests2()
  }



}
