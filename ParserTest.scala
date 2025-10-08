package spreadsheet

import Parser._

/** Tester for parsers. */
object ParserTest extends ParserTest0{

  // ============================================

  /** Tests on parsing CSV. */
  def csv() = {
    assert(CSVParser("3,4.5,,false,\"Hello, world\",\"\"\"Hello\",") == 
      List(
        IntValue(3), FloatValue(4.5F), Empty(), BoolValue(false),
        StringValue("Hello, world"), StringValue("\"Hello"), Empty()
      )
    )
    Failure.reset
    assert(CSVParser.line("3 three").isInstanceOf[Failure])
    println("CSV tests done")
  }

  /** Tests on parsing cells. */
  def cellTests() = {
    assert(CellParser("3") == IntValue(3))
    assert(CellParser("-3.5") == FloatValue(-3.5F))
    assert(CellParser("true") == BoolValue(true))
    assert(CellParser("false") == BoolValue(false))
    assert(CellParser("Hello, world") == StringValue("Hello, world"))
    assert(CellParser("\"Hello\"") == StringValue("Hello"))
    assert(CellParser("\"3\"") == StringValue("3"))

    assert(CellParser("\\Z") == StringValue("\\Z"))
    assert(CellParser("\"Hello").isInstanceOf[ParseError])
    assert(CellParser("\"\\Z\"").isInstanceOf[ParseError]) // .isInstanceOf[Failure])

    println("Cell tests done")
  }

// TODO: tests on cells.  Consider badly formed strings. e.g. " \Z "

  def main(args: Array[String]) = {
    // printErrors = true
    //printParseErrors = true
    ExpParserTest(); StatementParserTest();  csv(); cellTests()
    println("Done")
  }
}
