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
      "toString" -> toStringT
    ).map{ case (n,t) => (n,List(t)) }  ++
    List("-" -> negTs) ++ 
    selectorTypes

  /* Definitions. */

  private val headFn = 
    FunctionValue{ case List(l:ListValue) => l.head }
  private val tailFn = 
    FunctionValue{ case List(l:ListValue) => l.tail }
  private val isEmptyFn = 
    FunctionValue{ case List(l:ListValue) => l.isEmpty }
  private val notFn = FunctionValue{ case List(BoolValue(b)) => BoolValue(!b) }
  private val toIntFn = 
    FunctionValue{ case List(FloatValue(x)) => IntValue(x.toInt) }
  private val toFloatFn = 
    FunctionValue{ case List(IntValue(n)) => FloatValue(n.toFloat) }
  private val toStringFn = 
    FunctionValue{ case List(v) => StringValue(v.forError) }

  import NameExp.getName

  /** The selector functions. */
  private val selectorFns =
    for(i <- (1 to MaxArity).toList; a <- (2 max i) to MaxArity)
    yield{
      // Note: we have to use NameExp.getName to set the names appropriately,
      // except for the maximum arity.  This is a bit of a hack.
      val name = 
        if(i == MaxArity) s"get$i" else getName(s"get$i", a-(2 max i))
      name -> FunctionValue{
        case List(tv @ TupleValue(elems)) if tv.arity == a => elems(i-1)
          // Note: tuples use 1-based indexing, but lists use 0-based indexing.
      }
    }

  /** The negation functions. */
  private val negFns = List(
    getName("-",0) -> FunctionValue{ case List(IntValue(n)) => IntValue(-n) },
    getName("-",1) -> FunctionValue{ case List(FloatValue(x)) => FloatValue(-x) }
  )

  /** The built-in functions. */
  val builtIns =
    List(
      "head" -> headFn, "tail" -> tailFn, "isEmpty" -> isEmptyFn, "not" -> notFn,
      "toInt" -> toIntFn, "toFloat" -> toFloatFn, "!" -> notFn,
      "toString" -> toStringFn
    ) ++ 
      selectorFns ++ negFns

}
