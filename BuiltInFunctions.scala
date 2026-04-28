package spreadsheet

/** Types and definitions of built-in functions. */
object BuiltInFunctions{
  /* Types. */

  private val headT = {
    val t = TypeParam("A")
    FunctionType(List(("A", AnyTypeConstraint)), List(ListType(t)), t)
  }
  private val tailT = {
    val t = TypeParam("A")
    FunctionType(List(("A", AnyTypeConstraint)), List(ListType(t)), ListType(t))
  }
  private val isEmptyT = { 
    val t = TypeParam("A")
    FunctionType(List(("A", AnyTypeConstraint)), List(ListType(t)), BoolType)
  }
  private val notT = FunctionType(List(), List(BoolType), BoolType)
  private val toIntT = FunctionType(List(), List(FloatType), IntType)
  private val toFloatT = FunctionType(List(), List(IntType), FloatType)

  /** Type of "-": overloaded. */
  private val negTs = List(
    FunctionType(List(), List(IntType), IntType),
    FunctionType(List(), List(FloatType), FloatType)
  )

  /** The type of toString.  We treat is as polymorphic. */
  private val toStringT = FunctionType(
    List(("A", AnyTypeConstraint)), List(TypeParam("A")), StringType
  )
  /** sortBlockByColumn: (List[Column], List[Row], Column) => Unit. */
  private val sortBlockByColumnT = FunctionType(
    List(), List(ListType(ColumnType), ListType(RowType), ColumnType), UnitType)
  /** The type (Row, Row) => Boolean.*/
  private val rrbT = FunctionType( List(), List(RowType, RowType), BoolType )

  /** def sortBlockBy(cols: List[Column], rows: List[Row], 
    * before: Row => Row => Boolean): Unit */
  private val sortBlockByT = FunctionType(
    List(), List(ListType(ColumnType), ListType(RowType), rrbT), UnitType )


  import TupleType.MaxArity

  /** The selector functions, e.g. get2. */
  private val selectorTypes = 
    for(i <- (1 to MaxArity).toList) 
    yield s"get$i" -> ( // Defined on tuples of size i max 2 or larger
      for (a <- ((i max 2) to MaxArity).toList) // (T1,...,Ta) => Ti
      yield FunctionType(
        for(j <- (1 to a).toList) yield (s"A$j", AnyTypeConstraint),
        List(TupleType(for(j <- (1 to a).toList) yield TypeParam(s"A$j"))),
        TypeParam(s"A$i") ))

  /** The types of built-in functions. */
  val builtInTypes = 
    List("head" -> headT, "tail" -> tailT, "isEmpty" -> isEmptyT, "not" -> notT,
      "toInt" -> toIntT, "toFloat" -> toFloatT, "!" -> notT,
      "toString" -> toStringT, "sortBlockByColumn" -> sortBlockByColumnT,
      "sortBlockBy" -> sortBlockByT
    ).map{ case (n,t) => (n,List(t)) }  ++
    List("-" -> negTs) ++ 
    selectorTypes

  /* Definitions. */

  private def mkFunctionValue(f: PartialFunction[List[Value], Value]) = 
    FunctionValue((env: Environment) => f)

  private val headFn = 
    mkFunctionValue{ case List(l:ListValue) => l.head }
  private val tailFn = 
    mkFunctionValue{ case List(l:ListValue) => l.tail }
  private val isEmptyFn = 
    mkFunctionValue{ case List(l:ListValue) => l.isEmpty }
  private val notFn = mkFunctionValue{ case List(BoolValue(b)) => BoolValue(!b) }
  private val toIntFn = 
    mkFunctionValue{ case List(FloatValue(x)) => IntValue(x.toInt) }
  private val toFloatFn = 
    mkFunctionValue{ case List(IntValue(n)) => FloatValue(n.toFloat) }
  private val toStringFn = 
    mkFunctionValue{ case List(v) => StringValue(v.asString) }

  import NameExp.getName

  /** The selector functions. */
  private val selectorFns =
    for(i <- (1 to MaxArity).toList; a <- (2 max i) to MaxArity)
    yield{
      // Note: we have to use NameExp.getName to set the names appropriately,
      // except for the maximum arity.  This is a bit of a hack.
      val name = 
        if(i == MaxArity) s"get$i" else getName(s"get$i", a-(2 max i))
      name -> mkFunctionValue{
        case List(tv @ TupleValue(elems)) if tv.arity == a => elems(i-1)
          // Note: tuples use 1-based indexing, but lists use 0-based indexing.
      }
    }

  /** The negation functions. */
  private val negFns = List(
    getName("-",0) -> mkFunctionValue{ case List(IntValue(n)) => IntValue(-n) },
    getName("-",1) -> 
      mkFunctionValue{ case List(FloatValue(x)) => FloatValue(-x) }
  )

  // ========= Sorting functions

  /** Test whether there is any repeated row in rowNums, returning an error if
    * so; otherwise return null. */
  private def checkForRowRepetition(rowNums: Array[Int]): EvalError = {
    var i = 0; val numRows = rowNums.length; var result: EvalError = null
    while(i < numRows && result == null){
      var j = i+1; val r = rowNums(i)
      while(j < numRows && rowNums(j) != r) j += 1
      if(j < numRows) result = EvalError(s"Row #$r repeated in sort function")
      else i += 1
    }
    result
  }

  /** Reorder the block defined by cols and rows, into the order defined by
    * sortedRows. */
  private def copyCells(env: Environment, cols: Array[Int], rows: Array[Int],
    sortedRows: Array[Int]) 
  = {
    // Copy cells in sorted order.  The entry in position (i,j) is what should
    // end up in position (i,j) of the original block.
    val copy = Array.tabulate(cols.length, rows.length){ case (i,j) =>
      env.getCellInfo(cols(i), sortedRows(j)) }
    // Copy back into env.
    for(i <- 0 until cols.length; j <- 0 until rows.length)
      env.setCellInfo(cols(i), rows(j), copy(i)(j))
  }

  /** The sortBlockByColumn function.  Sorts the block defined by `cols` and
    * `rows` according to column `cv`.  Note that `rows` is expected to be
    * ordered without; but if not, subsequently the block will be ordered in
    * the order given by `rows`. */
  private val sortBlockByColumnFn = FunctionValue((env: Environment) => {
    case List(ListValue(cols), ListValue(rows), cv) =>
      assert(cols.forall(_.isInstanceOf[ColumnValue]))
      assert(rows.forall(_.isInstanceOf[RowValue]))
      val rowNums = rows.map{ case RowValue(r) => r }.toArray
      val colNums = cols.map{ case ColumnValue(c) => c }.toArray
      // Check no repetitions in `rows`
      var result: Value = checkForRowRepetition(rowNums)
      if(result != null) result
      else if(rows.isEmpty) UnitValue
      else{
        val ColumnValue(col) = cv
        // Check appropriate value in first cell in col
        val row0 = rowNums(0)
        val theType = env.getCell(col, row0).getType
        if(theType == EmptyType) 
            result = TypeError(s"Expected non-empty cell in ${cv.forError}$row0")
        else if(theType == ErrorType)
          result = UnitValue // do nothing in this case
        // Check remaining cells in col have type `theType`. 
        var i = 1
        while(i < rowNums.length && result == null){
          val row = rowNums(i); val cell = env.getCell(col,row)
          if(cell.getType == ErrorType) result = UnitValue
          else if(cell.getType != theType)
            result = TypeError(s"Expected $theType, found ${cell.forError}, "+
              s"in ${cv.forError}$row")
          else i += 1
        } // end of while loop
        if(result != null) result 
        else{
          // Order to sort rows into. 
          val sortedRows = rowNums.sortWith{ case (r1,r2) =>
            (env.getCell(col,r1) <= env.getCell(col,r2)) }
          copyCells(env, colNums, rowNums, sortedRows)
          UnitValue
        }
      } // end of else
  })

  /** Exception corresponding to the function fv, below, giving an error. */ 
  private case class ErrorException(err: ErrorValue) extends Exception

  /** The sortBlockBy function.  Sorts the block defined by `cols` and `rows`,
    * according to the criterion defined by `f: (Row, Row) => Boolean`. */
  private def sortBlockByFn = FunctionValue((env: Environment) => {
    case List(ListValue(cols), ListValue(rows: List[RowValue] @unchecked),
      fv: FunctionValue
    ) =>
      assert(cols.forall(_.isInstanceOf[ColumnValue]))
      assert(rows.forall(_.isInstanceOf[RowValue]))
      val rowNums = rows.map{ case RowValue(r) => r }.toArray
      val colNums = cols.map{ case ColumnValue(c) => c }.toArray
      // Check no repetitions in `rows`
      var result: Value = checkForRowRepetition(rowNums.toArray)
      if(result != null) result
      else if(rows.isEmpty) UnitValue
      else{
        val FunctionValue(f) = fv
        // Note: if f returns an error, sortWith below throws an
        // ErrorException, which gets caught.
        def compare(r1: Int, r2: Int) = 
          f(env)(List(RowValue(r1), RowValue(r2))) match{
            case BoolValue(b) => b
            case err: ErrorValue => throw ErrorException(err)
          }
        try{
          val sortedRows = rowNums.sortWith(compare _)
          copyCells(env, colNums, rowNums, sortedRows)
          UnitValue
        }
        catch{ case ErrorException(err) => err }
      }
  })

  /** The built-in functions. */
  val builtIns =
    List(
      "head" -> headFn, "tail" -> tailFn, "isEmpty" -> isEmptyFn, "not" -> notFn,
      "toInt" -> toIntFn, "toFloat" -> toFloatFn, "!" -> notFn,
      "toString" -> toStringFn, "sortBlockByColumn" -> sortBlockByColumnFn, 
      "sortBlockBy" -> sortBlockByFn
    ) ++ 
      selectorFns ++ negFns

}
