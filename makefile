CP = .:/home/gavin/Scala/lib/scala-swing_2.13-3.0.0.jar

DIR = spreadsheet

all: $(DIR)/ParserTest.class $(DIR)/SpreadsheetApp.class $(DIR)/TypeCheckerTest.class 

# ===== Language

# Types

$(DIR)/TypeConstraint.class: $(DIR)/TypeT.class

$(DIR)/FunctionType.class: $(DIR)/TypeConstraint.class

# Values

$(DIR)/Value.class: $(DIR)/Input.class $(DIR)/FunctionType.class

# Syntax

$(DIR)/HasExtent.class: $(DIR)/Input.class $(DIR)/Value.class

$(DIR)/Exp.class: $(DIR)/Value.class $(DIR)/HasExtent.class

$(DIR)/Statement.class: $(DIR)/Exp.class 

# Type checking basics

$(DIR)/Reply.class: $(DIR)/HasExtent.class

$(DIR)/Substitution.class: $(DIR)/FunctionType.class $(DIR)/Reply.class

$(DIR)/BuiltInFunctions.class: $(DIR)/Value.class $(DIR)/TypeConstraint.class

$(DIR)/TypeEnv.class: $(DIR)/TypeConstraint.class $(DIR)/Exp.class	\
  $(DIR)/BuiltInFunctions.class $(DIR)/Substitution.class

# Evaluation/execution

$(DIR)/EvaluationTypeChecker.class: $(DIR)/TypeEnv.class

$(DIR)/Environment.class: $(DIR)/EvaluationTypeChecker.class	\
  $(DIR)/BuiltInFunctions.class

$(DIR)/FunctionValue.class: $(DIR)/Exp.class $(DIR)/Environment.class

$(DIR)/Execution.class: $(DIR)/Statement.class $(DIR)/Environment.class	\
  $(DIR)/FunctionValue.class

# Type checking

$(DIR)/Unification.scala: $(DIR)/TypeEnv.class	\
  $(DIR)/EvaluationTypeChecker.class

$(DIR)/TypeChecker.class: $(DIR)/Unification.class $(DIR)/Substitution.class	\
  $(DIR)/Exp.class $(DIR)/FunctionValue.class $(DIR)/Statement.class

# Parsing and tests

$(DIR)/Parser.class: $(DIR)/Input.class

$(DIR)/StatementParser.class: $(DIR)/Parser.class $(DIR)/Statement.class	\
  $(DIR)/TypeConstraint.class $(DIR)/FunctionValue.class

$(DIR)/ParserTest.class: $(DIR)/StatementParser.class $(DIR)/Execution.class

$(DIR)/TypeCheckerTest.class: $(DIR)/TypeChecker.class	\
  $(DIR)/StatementParser.class

# ===== Model

$(DIR)/Model.class: $(DIR)/ViewT.class $(DIR)/StatementParser.class	\
  $(DIR)/TypeChecker.class $(DIR)/Execution.class

# ===== View

$(DIR)/Spreadsheet.class: $(DIR)/ViewT.class $(DIR)/Model.class

$(DIR)/View.class: $(DIR)/Model.class $(DIR)/Spreadsheet.class $(DIR)/ViewT.class

# ===== Top level

$(DIR)/SpreadsheetApp.class: $(DIR)/Spreadsheet.class $(DIR)/View.class

clean:
	rm $(DIR)/*.class *.class; fsc -shutdown

$(DIR)/%.class:     %.scala
	fsc -deprecation -cp $(CP) $<
