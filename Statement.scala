package spreadsheet

/** A Statement corresponds to either a definition of a directive in a
  * file. */
trait Statement extends HasExtent

/** Trait of declarations.  These can appear in expression blocks. */
trait Declaration extends Statement

// =======================================================

/** A simple Directive of the form `Cell(col,row) = exp`. */
// case class Directive(cell: CellExp, expr: Exp) extends Statement{
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
    extends Declaration

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
    extends Statement //  Declaration


/* Note: we can't include this in Exp.scala, because it builds on Statement. */

/** A block expression of the form { stmt_1; ...; stmt_n; exp }. */ 
case class BlockExp(stmts: List[Declaration], exp: Exp) extends Exp
