package spreadsheet

/** The result of a Parser. */
sealed abstract class ParseResult[+T]{
  /** Get the result associated with this.  Pre: this is a `Success`. */
  def get: T
}

/** A successful parse, giving `result`, with remaining input `rest`. */
case class Success[T](result: T, rest: Input) extends ParseResult[T]{
// TODO: add a field representing the start of this expression in the input
  def get = result
}

/** An unsuccessful parse on `in`, explained by `msg`. */ 
case class Failure(msg: String, in: Input) extends ParseResult[Nothing]{
  def get = ???
}

// ==================================================================

/** Combinatorial parsers.  Loosely based on Chapter 31 of Odersky et al. */
abstract class Parser[+A] extends (Input => ParseResult[A]){
  private val p = this

  /** Apply this to `in`.  Implementing classes must provide the definition. */
  def apply(in: Input): ParseResult[A]

  /** Apply this to `st`. */
  def apply(st: String): ParseResult[A] = apply(new Input(st))

  /** The sequential composition of this and `q` (with no intervening space). */
  def ~~ [B](q: => Parser[B]) = new Parser[(A,B)]{
    def apply(in: Input) = p(in) match{
      case Success(r1, in1) => q(in1) match{
        case Success(r2, rest) => Success((r1,r2), rest)
        case Failure(msg, in2) => Failure(msg, in2)
      }
      case Failure(msg, in2) => Failure(msg, in2)
    }
  }

  /** The sequential composition of this and `q`, possibly with white space
    * between. */
  def ~ [B](q: => Parser[B]): Parser[(A,B)] =
    this ~~ (Parser.consumeWhite ~~ q) > { case (l, (_,r)) => (l,r) }
    // (this ~~ Parser.consumeWhite) ~~ q > { case ((l,_),r) => (l,r) }

  /** Sequential composition of this and `q`, possibly with white space between,
    * returning the result of this. */
  def <~ [B](q: => Parser[B]): Parser[A] = this ~ q > { case (x,y) => x }

  /** Sequential composition of this and `q`, possibly with white space between,
    * returning the result of `q`. */
  def ~> [B](q: => Parser[B]): Parser[B] = this ~ q > { case (x,y) => y }

  /** Choice between this and `q`. */
  def | [B >: A] (q: Parser[B]) = new Parser[B]{
    def apply(in: Input) = p(in) match{
      case s1: Success[A] => s1
      case failure => q(in)
    }
  }

  /** Parser that applies `f` to the result of this. */
  def > [B] (f: A => B) = new Parser[B]{
    def apply(in: Input) = p(in) match{
      case Success(r1, in1) => Success(f(r1), in1)
      case Failure(msg, in2) => Failure(msg, in2)
    }
  }

  /** Parser to apply this repeatedly. */
  def * : Parser[List[A]] = Parser.repeat(this)

  /** Parser to apply this one or more times. */
  def + : Parser[List[A]] = this ~ this.* > { case (d,ds) => d::ds }
}

// =======================================================

object Parser{
  /** A parser that succeeds with result `x` (consuming no input. */
  def success[A](x: A) = new Parser[A]{
    def apply(in: Input) = Success(x, in)
  }

  def failure(msg: String) = new Parser[Nothing]{
    def apply(in: Input) = Failure(msg, in)
  }

  /** A parser for the literal `st`. */
  def lit(st: String) = new Parser[String]{
    def apply(in: Input) =
      if(in.startsWith(st)) Success(st, in.advance(st.length))
      else Failure(s"expected \"$st\"", in)
  }

  /** A parser that consumes the first character of its input if that character
    * satisfies `p`. */
  def spot(p: Char => Boolean) = new Parser[Char]{
    def apply(in: Input) = {
      if(in.isEmpty) Failure(s"Unexpected end of input", in)
      else{
        val x = in.head
        if(p(x)) Success(x, in.advance(1))
        else Failure(s"Unexpected character $x", in)
      }
    }
  }

  // ===== Operations on parsers

  /** Adapt function `f` over two arguments to operate on a pair. */
  def toPair[A,B,C](f: (A,B) => C)(pair: (A,B)): C = {
    val (a,b) = pair; f(a,b) 
  }

  /** A parser that applies p repeatedly, possibly with intermediate white
    * space. */
  def repeat[A](p: => Parser[A]): Parser[List[A]] = (
    p ~ repeat(p) > toPair(_::_)
    | success(List())
  )

  /** A parser that applies p repeatedly, with no intermediate white space. */
  def repeat1[A](p: => Parser[A]): Parser[List[A]] = (
    p ~~ repeat1(p) > toPair(_::_) 
    | success(List())
  )

  /** A parser that optionally applies `p`. */
  def opt[A](p: => Parser[A]): Parser[Option[A]] = 
    (p > (x => Some(x))) | success(None)

  /** A choice between a list of parsers. */
  def | [A](ps: => List[Parser[A]]): Parser[A] = 
    if(ps.isEmpty) failure("Empty list of parsers")
    else ps.head | (|(ps.tail))

  /** A parser that applies `p` to an input in parentheses. */
  def inParens[A](p: => Parser[A]): Parser[A] =  lit("(") ~> p <~ lit(")")

  /** Parse `input` using `p`.  If successful, return `Left` applied to the
    * result; otherwise return `Right` applied to an error message. */
  def parseWith[A](p: Parser[A], input: String): Either[A, String] = 
    p(new Input(input).dropWhite) match{
      case Success(result, rest) =>
        if(rest.dropWhite.isEmpty) Left(result)
        else Right(s"Parser error: extra lost: \"$rest\"")
      case Failure(msg, _) => Right(msg)
    }

  /** Parse `input` using `p`, allowing initial and trailing white space.
    * Expect all the input to be consumed, and return the result. */
  def parseAll[A](p: Parser[A], input: String) = 
    parseWith(p, input) match{
      case Left(result) => result
      case Right(msg) => println(msg); sys.exit()
    }
    // p(new Input(input).dropWhite) match{
    //   case Success(result, rest) =>
    //     if(rest.dropWhite.isEmpty) result
    //     else{ println(s"Parser error: extra lost: \"$rest\""); sys.exit() }
    //   case Failure(msg, _) => println(msg); sys.exit()
    // }


  // ========= Specific parsers

  /** A parser that consumes all white space at the start of its input. */
  val consumeWhite = new Parser[Unit]{
    def apply(in: Input) = Success((), in.dropWhite)
  }

  /** A parser that succeeds if at the end of the input. */
  def atEnd = new Parser[Unit]{
    def apply(in: Input) =
      if(in.isEmpty) Success((), in) else Failure("Not at end of input", in)
  }

  /** A parser that consumes and returns all its input. */
  def all = new Parser[String]{
    def apply(in: Input) = {
      val st = in.toString; Success(st, in.advance(st.length))
    }
  }

  /** A parser for an Int. */
  val int: Parser[Int] = {
    /* Convert `d::ds` to an Int. */
    def mkInt(d: Char, ds: List[Char]): Int = {
      var ds1 = ds; var x = d-'0'
      while(ds1.nonEmpty){ x = 10*x+(ds1.head-'0'); ds1 = ds1.tail }
      x
    }
    // Parser for positive ints
    val posInt = spot(_.isDigit) ~~ repeat1(spot(_.isDigit)) > toPair(mkInt) 
    lit("-") ~~ posInt > { case(_,n) => -n} | posInt
  }

  /** A parser for a Scala-style identifier: (a-z)(a-zA-Z0-9)*. */
  val name: Parser[String] =
    spot(_.isLower) ~~ repeat1(spot(_.isLetterOrDigit)) > 
      toPair(_::_) > (_.mkString) 

  val name0 =
    spot(_.isLower) ~~ repeat1(spot(_.isLetterOrDigit)) 
  // =====

  /** Some tests. */
  def main(args: Array[String]) = {
    // Generic tests on combinators
    val p = lit("Hello") ~~ (lit(" world") | lit(" all")) > toPair(_+_)
    assert(parseAll(p, "Hello world") == "Hello world")

    // val p1 = ws("Hello") ~ (ws("world") | ws("all")) > 
    //   { case (s1,s2) => s1+"#"+s2 }
    // assert(parseAll(p1, " \n Hello \t  world  ") == "Hello#world")
    // println(p1(new Input(" \n Hello \t  world  ")))

    val p2 = lit("Hello") ~ (lit("world") | lit("all")) > 
      { case (s1,s2) => s1+"#"+s2 }
    //println("2"+p2(new Input("Hello \t \n  world  ")))
    //println(parseAll(p2, "  Hello \t \n  world  ")+"$")
    assert(parseAll(p2, "  Hello \t \n  world  ") == "Hello#world")

    // Tests on int
    assert(parseAll(int, "1234") == 1234)   //println(int(new Input("1234")))
    assert(parseAll(int, "-234") == -234) // println(int(new Input("-1234")))
    // println("X"); println(int("one"))
    assert(int("one").isInstanceOf[Failure])   // println(int(new Input("one")))

    assert(parseAll(name, " fooBar5 ") == "fooBar5")
    assert(name("foo!Bar").get == "foo") //   println(name("foo!Bar"))
    assert(name("FooBar").isInstanceOf[Failure])  // 
    println(name("foo bar"))
    println(name0("foo bar"))

    println("Done")
  }

}
