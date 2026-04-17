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
// trait Declaration extends Statement

// =======================================================

/** A simple Directive of the form `Cell(col,row) = exp`. */
case class Directive(col: Exp, row: Exp, expr: Exp) extends Statement{
  override def toString = s"Cell($col,$row) = $expr"
}

// =======================================================

/** A pattern in a ValueDeclaration. */
trait ValPattern{
  /** The names bound in the pattern. */
  def names: List[String]
}

/** A simple name as a pattern. */
case class NamePattern(name: String) extends ValPattern{
  def names = List(name)
}

/** A declaration of the form `val name = exp`. */
case class ValueDeclaration(pattern: ValPattern, exp: Exp) extends Statement



// =======================================================

object FunctionDeclaration{
  /** A list of formal parameters of a function declaration.  A curried function
    * declaration will have a List of ParameterLists. */
  type ParameterList = List[(String, TypeT)]
}

import FunctionDeclaration.ParameterList


/** The declaration of a function "def name[tparams](args) = exp".  ort =
  * Some(rt) if an explicit return type rt is given.*/
case class FunctionDeclaration(
  name: String, tParams: List[FunctionType.TypeParameter], 
  params: List[ParameterList], ort: Option[TypeT], body: Exp)
    extends Statement{
  private var index = -1

  def setIndex(ix: Int) = index = ix

  /** The name against which the name of this function is stored in the
    * evaluation environment. */
  def getName = NameExp.getName(name, index)

  /** The types of the parameters of this. */
  def paramTs: List[List[TypeT]] = params.map(_.map(_._2))

  /** A FunctionType object representing the type of this function.  If the
    * return type is undefined, uses null. */
  def mkFunctionType = 
    FunctionType(tParams, params.head.map(_._2), 
      mkFunctionType1(params.tail, if(ort.isDefined) ort.get else null))

  /** A FunctionType object representing the type of this function with return
    * type rt.  Requires that the return type is not defined. */
  def mkFunctionType(rt: TypeT) = {
    require(ort.isEmpty)
    FunctionType(tParams, params.head.map(_._2), 
      mkFunctionType1(params.tail, rt))
  }

  /** Make the type of a curried function with parameters ps and result type
    * rt. */
  private def mkFunctionType1(ps: List[ParameterList], rt: TypeT): TypeT = 
    if(ps.isEmpty) rt
    else FunctionType(List(), ps.head.map(_._2), mkFunctionType1(ps.tail, rt))
}

// =======================================================


/** A "for" statement, for(binders){ stmts }. */
case class ForStatement(qualfiers: List[Qualifier], stmts: List[Statement]) 
    extends Statement

// =======================================================

/** An assertion.  */
case class Assertion(condition: Exp) extends Statement

/** An assertion with an error message. */
case class Assertion2(condition: Exp, msg: Exp) extends Statement

// =======================================================

/** The call of expression e, which is expected to be a function call
  * returning Unit. */
case class CallStatement(e: Exp) extends Statement

/* Note: we can't include this in Exp.scala, because it builds on Statement. */

/** A block expression of the form { stmt_1; ...; stmt_n; exp }; or
  * { stmt_1; ...; stmt_n } if exp = null. */ 
case class BlockExp(stmts: List[Statement], exp: Exp) extends Exp
 
 
