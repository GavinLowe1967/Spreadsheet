package spreadsheet

/** A Statement corresponds to either a definition of a directive in a
  * file. */
trait Statement extends HasExtent

object Statement{
  /** Get all the FunctionDeclarations from stmts. */
  // def getFnDecs(stmts: List[Statement]): List[FunctionDeclaration] = 
  //   for(fn @ FunctionDeclaration(_,_,_,_,_) <- stmts) yield fn 
}

/** Trait of declarations.  These can appear in expression blocks. */
trait Declaration extends Statement

// =======================================================

/** A simple Directive of the form `Cell(col,row) = exp`. */
case class Directive(col: Exp, row: Exp, expr: Exp) extends Statement{
  override def toString = s"Cell($col,$row) = $expr"
}

// =======================================================

/** A declaration of the form `val name = exp`. */
case class ValueDeclaration(name: String, exp: Exp) extends Declaration

// =======================================================

/** The declaration of a function "def name[tparams](args): rt = exp". */
case class FunctionDeclaration(
  name: String, tParams: List[FunctionType.TypeParameter], 
  params: List[(String, TypeT)], rt: TypeT, body: Exp)
    extends Declaration{
  private var index = -1

  def setIndex(ix: Int) = index = ix

  /** The name against which the name of this function is stored in the
    * evaluation environment. */
  def getName = NameExp.getName(name, index)

  /** The types of the parameters of this. */
  def paramTs = params.map(_._2)

  /** A FunctionType object representing the type of this function. */
  def mkFunctionType = FunctionType(tParams, paramTs, rt)
}

// =======================================================

/** A binder for a "for" statement. */
trait Binder
/** A generator, "name <- exp". */
case class Generator(name: String, exp: Exp) extends Binder
// IMPROVE: more general forms of pattern matching?
/** A filter, "if exp". */
case class Filter(exp: Exp) extends Binder

/** A for statement, for(binders){ stmts }. */
case class ForStatement(binders: List[Binder], stmts: List[Statement]) 
    extends Statement

/* Note: we can't include this in Exp.scala, because it builds on Statement. */

/** A block expression of the form { stmt_1; ...; stmt_n; exp }. */ 
case class BlockExp(stmts: List[Declaration], exp: Exp) extends Exp
