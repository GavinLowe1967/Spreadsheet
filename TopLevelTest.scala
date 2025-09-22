package spreadsheet

/** A top-level test.  This loads script test.dir and sheet test.csv, and
  * checks that cells are as expected. */
object TopLevelTest{
  // Initialise Model
  val Height = 100; val Width = 26
  private val model = new Model(Height,Width); model.setView(DummyView)
  val cells = model.cells; val calculated = model.calculated

  /** Check cells(c,r) contains a calculated value. */
  def checkCalculated(c: Int, r: Int) = 
    assert(model.calculated(c)(r),
      s"Expected calculated value in #${ColumnValue.getName(c)}$r")

  /** Check cells(c,r) contains e. */
  def checkExpected(c: Int, r: Int, e: Value) = 
    assert(cells(c)(r) == e,
      s"Expected $e found ${cells(c)(r)} in #${ColumnValue.getName(c)}$r")

  // Column names
  val A = 0; val B = 1; val C = 2; val D = 3

  /** Values expected to be provided as inputs. */
  val expectedInputs = List[(Int,Int,Value)](
    (A,1,IntValue(4)), (A,2,IntValue(5)), (A,3,IntValue(6))
  )

  /** List of expected values, with columns and rows. */
  val expected = List[(Int,Int,Value)](
    (A,0,FloatValue(6.0F)), (B,0,IntValue(10)),
    (B,1,IntValue(24)), (C,1,IntValue(24)),
    (B,3,IntValue(720)), (C,3,IntValue(720)),
    (A,5,BoolValue(true)), (A,6, BoolValue(true))
  )

  /** Lit of cells where type errors are expected. */
  val expectedTypeErrs = List((B,4), (C,4))

  /** List of cells where evaluation errors are expected. */
  val expectedEvalErrs = List((C,0), (D,0))

  /** Main function. */
  def main(args: Array[String]) = {
    model.loadScript("test.dir", "test.csv")
    val filled = Array.ofDim[Boolean](Width,Height)

    // Check inputs are as expected.
    for((c,r,e) <- expectedInputs){
      filled(c)(r) = true
      assert(!model.calculated(c)(r),
        s"Expected input value in #${ColumnValue.getName(c)}$r")
      checkExpected(c,r,e)
    }
    // Check calculated values are as expected. 
    for((c,r,e) <- expected){ 
      filled(c)(r) = true; checkCalculated(c,r); checkExpected(c,r,e) 
    }
    // Check type errors are as expected. 
    for((c,r) <- expectedTypeErrs){
      filled(c)(r) = true; checkCalculated(c,r)
      assert(cells(c)(r).isInstanceOf[TypeError])
    }
    // Check evaluation errors are as expected. 
    for((c,r) <- expectedEvalErrs){
      filled(c)(r) = true; checkCalculated(c,r)
      assert(cells(c)(r).isInstanceOf[EvalError])
    }
    // Check remaining cells are Empty. 
    for(c <- 0 until Width; r <- 0 until Height; if !filled(c)(r))
      assert(cells(c)(r).getType == EmptyType)
  }

}
