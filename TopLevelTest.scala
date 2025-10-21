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
  val A = 0; val B = 1; val C = 2; val D = 3; val E = 4; val F = 5

  /** Values expected to be provided as inputs. */
  val expectedInputs = List[(Int,Int,Value)](
    (A,1,IntValue(4)), (A,2,IntValue(5)), (A,3,IntValue(6)), 
    (A,7,StringValue("3")), (A,8,StringValue("\"Hello\""))
  )

  /** List of expected values, with columns and rows. */
  val expected = List[(Int,Int,Value)](
    // #B0 = sum([1,2,3,4]): Int + sum([]) + sum([]: List[Int])
    // val hello = "Hello"; #E0 = hello
    // #F0 = apply(double, 3.0) : Float;
    (B,0,IntValue(10)), (E,0,StringValue("Hello")), (F,0,FloatValue(6.0F)), 
    // Factorials, rows 1-4 
    // for(r <- #1 to #4; c <- [#B, #C]; if r != #2){
    //   Cell(c, r) = fact(Cell(#A,r))
    // }
    (B,1,IntValue(24)), (C,1,IntValue(24)),
    (B,3,IntValue(720)), (C,3,IntValue(720)),
    // val firstEmpty = firstEmptyInCol(#A, #1) // Should give #4
    // Cell(#D, firstEmpty) = "first empty"
    (D,4,StringValue("first empty")),
    // More in row 6
    // val ff = fact: Int => Int
    // #A6 = ff(3)
    // #B6 = 3+#A1 // untyped cell read.
    (A,6,IntValue(6)), (B,6,IntValue(7)), 
    // #C6 = if(false) 3 else #A1 // Untyped cell read.
    // #D6 = g(1) // should be 4
    (C,6,IntValue(4)), (D,6,IntValue(4)),
    // val flag = true
    // for(r <- #7 to #8; if r == #6 || flag){
    //   Cell(#B, r) = flag; val flag = false
    // }
    (B,7,BoolValue(true)), (B,8, BoolValue(true)),
    // for(r <- 2 to 2; r <- #5+r to #6+r) Cell(#C, r) = 6
    (C,7,IntValue(6)), (C,8,IntValue(6))  
  )

  /** Lit of cells where type errors are expected. */
  val expectedTypeErrs = List((B,4), (C,4))

  /** List of cells where evaluation errors are expected. */
  val expectedEvalErrs = List((C,0), (D,0))
    // #C0 = 1/0: Int // Evaluation error
    // #D0 = 3; #D0 = 4.0 // Evaluation error

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
      assert(cells(c)(r).getType == EmptyType, 
        s"Found ${cells(c)(r)} in #$c#$r, expected empty cell.")

    println("All tests passed!")
  }

}
