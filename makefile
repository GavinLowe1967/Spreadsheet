CP = .:/home/gavin/Scala/lib/scala-swing_2.13-3.0.0.jar

DIR = spreadsheet

all: $(DIR)/ParserTest.class $(DIR)/SpreadsheetApp.class $(DIR)/TypeCheckerTest.class 

# ===== Language

# Types

$(DIR)/TypeConstraint.class: $(DIR)/TypeT.class

$(DIR)/FunctionType.class: $(DIR)/TypeConstraint.class

# Values

$(DIR)/Value.class: $(DIR)/Input.class $(DIR)/FunctionType.class

$(DIR)/BuiltInFunctions.class: $(DIR)/Value.class $(DIR)/TypeConstraint.class

$(DIR)/HasExtent.class: $(DIR)/Input.class $(DIR)/Value.class

$(DIR)/Reply.class: $(DIR)/HasExtent.class

# Expression syntax

$(DIR)/Exp.class: $(DIR)/Value.class $(DIR)/Reply.class

# Type checking

$(DIR)/Substitution.class: $(DIR)/FunctionType.class $(DIR)/Reply.class

$(DIR)/TypeEnv.class: $(DIR)/TypeConstraint.class $(DIR)/Exp.class $(DIR)/BuiltInFunctions.class $(DIR)/Substitution.class

$(DIR)/EvaluationTypeChecker.class: $(DIR)/TypeEnv.class

$(DIR)/Unification.scala: $(DIR)/TypeEnv.class	\
  $(DIR)/EvaluationTypeChecker.class

# ======

$(DIR)/Environment.class: $(DIR)/Value.class \
  $(DIR)/EvaluationTypeChecker.class  $(DIR)/BuiltInFunctions.class

$(DIR)/Statement.class: $(DIR)/Exp.class $(DIR)/ViewT.class $(DIR)/Environment.class

$(DIR)/FunctionDeclaration.class: $(DIR)/Exp.class $(DIR)/Statement.class $(DIR)/Environment.class

$(DIR)/BlockExp.class: $(DIR)/Statement.class

$(DIR)/Execution.class: $(DIR)/BlockExp.class $(DIR)/Environment.class

$(DIR)/TypeChecker.class: $(DIR)/Unification.class $(DIR)/Substitution.class	\
  $(DIR)/Exp.class $(DIR)/FunctionValue.class $(DIR)/BlockExp.class

# Parsing

$(DIR)/Parser.class: $(DIR)/Input.class

$(DIR)/StatementParser.class: $(DIR)/Parser.class $(DIR)/Value.class	\
  $(DIR)/BlockExp.class $(DIR)/TypeConstraint.class

$(DIR)/ParserTest.class: $(DIR)/StatementParser.class


$(DIR)/TypeCheckerTest.class: $(DIR)/TypeChecker.class $(DIR)/StatementParser.class

# ===== Model

$(DIR)/Model.class: $(DIR)/ViewT.class $(DIR)/StatementParser.class   $(DIR)/TypeChecker.class $(DIR)/Execution.class

# ===== View

$(DIR)/Spreadsheet.class: $(DIR)/StatementParser.class $(DIR)/ViewT.class	\
  $(DIR)/Model.class

$(DIR)/View.class: $(DIR)/Model.class $(DIR)/Spreadsheet.class $(DIR)/ViewT.class

# ===== Top level

$(DIR)/SpreadsheetApp.class: $(DIR)/Spreadsheet.class $(DIR)/View.class

clean:
	rm $(DIR)/*.class *.class; fsc -shutdown

$(DIR)/%.class:     %.scala
	fsc -deprecation -cp $(CP) $<
