package spreadsheet

/** The result of a Parser. */
sealed abstract class ParseResult[+T]{
  /** Can this be backtracked? */
  var backtrackable = true

  /** This parser with backtrackable information conjuncted with that of p. */
  def withBacktrack[A](p: ParseResult[A]) = { 
    backtrackable &&= p.backtrackable; this 
  }
}

/** A successful parse, giving `result`, with remaining input `rest`. */
case class Success[T](result: T, rest: Input) extends ParseResult[T]{
  def get = result
}

/** An unsuccessful parse on `in`, explained by `msg`. */ 
case class Failure(msg: String, in: Input) extends ParseResult[Nothing]{
  import Failure.lastFailure

  lastFailure match{
    case Some(f) => if(in >= f.in) lastFailure = Some(this)
    case None => lastFailure = Some(this)
  }

  /** Whichever of this and other that is most advanced. */
  def max(other: Failure) = 
    if(this.in >= other.in) this else other
}

object Failure{
  var lastFailure: Option[Failure] = None

  def reset = lastFailure = None
}

// ==================================================================

/** Combinatorial parsers.  Loosely based on Chapter 31 of Odersky et al. */
abstract class Parser[+A] extends (Input => ParseResult[A]){
  private val p = this

  /** Apply this to `in`.  Implementing classes must provide the definition. */
  def apply(in: Input): ParseResult[A]

  /** Apply this to `st`. */
  def apply(st: String): ParseResult[A] = { 
    Failure.reset; apply(new Input(st).dropWhite)
  }

  /** The sequential composition of this and `q` (with no intervening space). */
  def ~~ [B](q: => Parser[B]) = new Parser[(A,B)]{
    def apply(in: Input) = p(in) match{
      case s1 @ Success(r1, in1) => 
        val p2 = q(in1)
        (p2 match{
          case Success(r2, rest) => Success((r1,r2), rest)
          case Failure(msg, in2) => Failure(msg, in2)
        }).withBacktrack(s1).withBacktrack(p2)
      case f: Failure => f
    }
  }

  /** The sequential composition of this and `q` (with no intervening space),
    * not allowing subsequent backtracking. */
  def ~~! [B](q: => Parser[B]) = new Parser[(A,B)]{
    def apply(in: Input) = p(in) match{
      case s1 @ Success(r1, in1) => 
        val p2 = q(in1)
        val res = p2 match{
          case Success(r2, rest) => Success((r1,r2), rest)
          case Failure(msg, in2) => Failure(msg, in2)
        } 
        res.backtrackable = false; res
      case f: Failure => f
    }
  }
 
  /** The sequential composition of this and `q` (with no intervening space),
    * returning the result of `q`. */
  def ~~> [B](q: => Parser[B]): Parser[B] = this ~~ q > { case (x,y) => y }

  /** The sequential composition of this and `q` (with no intervening space),
    * returning the result of this. */
  def <~~ [B](q: => Parser[B]): Parser[A] = this ~~ q > { case (x,y) => x }

  /** The sequential composition of this and `q`, possibly with white space
    * between. */
  def ~ [B](q: => Parser[B]): Parser[(A,B)] =
    this ~~ (Parser.consumeWhite ~~> q) 

  /** The sequential composition of this and q, possibly with white space
    * between, allowing no backtracking. */
  def ~! [B](q: => Parser[B]): Parser[(A,B)] =
    this ~~! (Parser.consumeWhite ~~ q) > { case (l, (_,r)) => (l,r) }

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
      case f1: Failure => 
        if(f1.backtrackable) q(in) match{
          case s2: Success[B] => s2
          case f2 @ Failure(msg2, in2) => f2 // f1.max(f2)
        }
        else f1
    }
  }

  /** Parser that applies `f` to the result of this. */
  def > [B] (f: A => B) = new Parser[B]{
    def apply(in: Input) = p(in) match{
      case s @ Success(r1, in1) => Success(f(r1), in1).withBacktrack(s)
      case f @ Failure(msg, in2) => Failure(msg, in2).withBacktrack(f)
    }
  }

  /** Parser that checks result of this satisfies f. */
  def ? (f: A => Boolean) = new Parser[A]{
    def apply(in: Input) = p(in) match{
      case s @ Success(r1, in1) => 
        if(f(r1)) s else Failure(s"Unexpected token", in1)
      case f: Failure => f
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

  /** A parser that fails, with message `msg`. */
  def failure(msg: String) = new Parser[Nothing]{
    def apply(in: Input) = Failure(msg, in)
  }

  /** A parser for the literal `st`.  Note: this normally shouldn't be used for
    * literals starting with a letter, is it would consume part of a variable
    * name that starts with `lit`: use `keyword` in such cases. */
  def lit(st: String) = new Parser[String]{
    assert(st.nonEmpty && !st.head.isLetter)
    def apply(in: Input) =
      if(in.startsWith(st)) Success(st, in.advance(st.length))
      else Failure(s"Expected \"$st\"", in)
  }

  //def lit(st: String) = {assert(st.nonEmpty && !st.head.isLetter); lit1(st) }

  /** A parser for the keyword `word`.  This checks that the next character is
    * not alphanumeric, to avoid going wrong on variable names that are an
    * extention of a keyword, e.g. "truely". */
  def keyword(word: String) = new Parser[String]{
    val len = word.length
    def apply(in: Input) =
      if(in.startsWith(word) && (in.length == len || !in(len).isLetterOrDigit))
        Success(word, in.advance(len))
      else Failure(s"Expected \"$word\"", in)
  }

//lit(word) <~~ testNextNot(c => c.isLetterOrDigit)


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

  /** A parser that tests whether the next character of its input satisfies `p`,
    * without consuming that input. */
  def testNext(p: Char => Boolean) = new Parser[Char]{
    def apply(in: Input) = 
      if(in.isEmpty) Failure(s"End of input", in)
      else{
        val x = in.head
        if(p(x)) Success(x, in) else Failure(s"Unexpected character $x", in)
      }
  }

  /** A parser that tests whether the next character of its input does not
    * satisfy `p`, without consuming that input.  Succeeds on the empty
    * input. */
  def testNextNot(p: Char => Boolean) = new Parser[Char]{
    def apply(in: Input) = 
      if(in.isEmpty) Success(null.asInstanceOf[Char], in)
      else{
        val x = in.head
        if(!p(x)) Success(x, in) else Failure(s"Unexpected character $x", in)
      }
  }

  /** A parser that consumes one character. */
  def char: Parser[Char] = spot(_ => true)

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

  /** A parser that expects to find a sequence of expressions matching `p`,
    * separated by `sep`. */
  def repSep[A](p: Parser[A], sep: String): Parser[List[A]] = (
    p ~ repeat(lit(sep) ~> p) > toPair(_::_)
    | success(List())
  )

  /** A parser that expects to find a non-empty sequence of expressions matching
    * `p`, separated by `sep`. */
  def repSepNonEmpty[A](p: Parser[A], sep: String): Parser[List[A]] =
    (p ~ (lit(sep) ~> repSep(p, sep) | success(List())) ) > toPair(_ :: _)


  // /** Parse repeatedly with `p`, separated by `sep`, until `endMarker` is
  //   * reached. */
  // def repeatUntil[A,B,C](p: Parser[A], sep: Parser[B], endMarker: Parser[C])
  //     : Parser[(List[A], C)] = (
  //   p ~~ (
  //     sep ~> repeatUntil(p, sep, endMarker) 
  //     |
  //     consumeWhite ~> endMarker > { end => (List[A](), end) }
  //   ) > { case (r1, (rs,end)) => (r1::rs, end) }
  // )

  /** A parser that optionally applies `p`. 
    * Note: when sequencing this, use ~~ or similar on the left to avoid 
    * unintentionally consuming white space.  Instead, its argument should 
    * consume leading white space, so typically will be of the form
    * "consumeWhite ~~> ...". */
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
  def parseWith[A](p: Parser[A], input: String): Either[A, String] = {
    p(input) match{ 
      case Success(result, rest) =>
        if(rest.dropWhite.isEmpty) Left(result)
        else Right(s"Parser error: extra lost: \"$rest\"")
      case f: Failure => 
        val Some(Failure(msg, in)) = Failure.lastFailure
        val (lineNum, colNum, currLine) = in.getCurrentLine
        Right(s"$msg at line $lineNum: \n$currLine\n${" "*colNum}^")
    }
  }

  /** Parse `input` using `p`, allowing initial and trailing white space.
    * Expect all the input to be consumed, and return the result. */
  def parseAll[A](p: Parser[A], input: String) = 
    parseWith(p, input) match{
      case Left(result) => result
      case Right(msg) => println(msg); sys.exit()
    }

  // ========= Specific parsers

  /** Parser that consumes spaces and tabs but not newlines. */
  def consumeWhiteNoNL: Parser[String] = {
    def ws(c: Char) = c == ' ' || c == '\t'
    repeat1(spot(ws)) > (_.mkString)
  }

  /** Parser that consumes white space up to the end of the line. */
  def toLineEnd: Parser[String] = 
    consumeWhiteNoNL ~~ lit("\n") > toPair(_+_)

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

  /** Convert `d::ds` to an Int. */
  private def mkInt(d: Char, ds: List[Char]): Int = {
    var ds1 = ds; var x = d-'0'
    while(ds1.nonEmpty){ x = 10*x+(ds1.head-'0'); ds1 = ds1.tail }
    x
  }

  /** Parser for a positive Int. */
  val posInt = spot(_.isDigit) ~~ repeat1(spot(_.isDigit)) > toPair(mkInt)

  /** A parser for an Int. */
  val int: Parser[Int] = lit("-") ~~ posInt > { case(_,n) => -n} | posInt

  /** A parser for a Scala-style identifier: (a-zA-Z)(a-zA-Z0-9)*. */
  val name: Parser[String] =
    spot(_.isLetter) ~~ repeat1(spot(_.isLetterOrDigit)) > 
      toPair(_::_) > (_.mkString) 

  /** A parser for a name starting with an upper-case letter.  Used for names of
    * types. */
  val upperName: Parser[String] = 
    spot(_.isUpper) ~~ repeat1(spot(_.isLetterOrDigit)) > 
      toPair(_::_) > (_.mkString) 

  /** Some tests. */
  def main(args: Array[String]) = {
    // Generic tests on combinators
    val p = lit("Hello") ~~ (lit(" world") | lit(" all")) > toPair(_+_)
    assert(parseAll(p, "Hello world") == "Hello world")

    val p2 = lit("Hello") ~ (lit("world") | lit("all")) > 
      { case (s1,s2) => s1+"#"+s2 }
    assert(parseAll(p2, "  Hello \t \n  world  ") == "Hello#world")

    // Tests on int
    assert(parseAll(int, "1234") == 1234)   //println(int(new Input("1234")))
    assert(parseAll(int, "-234") == -234) // println(int(new Input("-1234")))
    // println("X"); println(int("one"))
    assert(int("one").isInstanceOf[Failure])   // println(int(new Input("one")))

    assert(parseAll(name, " fooBar5 ") == "fooBar5")
    assert(name("foo!Bar").asInstanceOf[Success[String]].get == "foo") 
    assert(name("FooBar").isInstanceOf[Failure])  // 
    println(name("foo bar"))

    println("Done")
  }

}
