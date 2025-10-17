package spreadsheet

/** Run all the tests. */
object AllTests{
  def main(args: Array[String]) = {
    ParserTest.main(Array())
    TypeCheckerTest.main(Array())
    TopLevelTest.main(Array())
  }
}
