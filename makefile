CP = .:/home/gavin/Scala/lib/scala-swing_2.13-3.0.0.jar

DIR = spreadsheet

all: $(DIR)/AllTests.class $(DIR)/SpreadsheetApp.class 

# ===== Language

# Types

$(DIR)/TypeConstraint.class: $(DIR)/TypeT.class

$(DIR)/FunctionType.class: $(DIR)/TypeConstraint.class

# Values

$(DIR)/Value.class: $(DIR)/Input.class $(DIR)/FunctionType.class

$(DIR)/BuiltInFunctions.class: $(DIR)/Value.class $(DIR)/TypeConstraint.class

# Syntax

$(DIR)/HasExtent.class: $(DIR)/Input.class $(DIR)/Value.class

$(DIR)/Exp.class: $(DIR)/Value.class $(DIR)/HasExtent.class

$(DIR)/Statement.class: $(DIR)/Exp.class 

# Evaluation/execution

$(DIR)/EvaluationTypeEnv.class: $(DIR)/TypeConstraint.class

$(DIR)/Reply.class: $(DIR)/HasExtent.class

$(DIR)/EvaluationTypeChecker.class: $(DIR)/EvaluationTypeEnv.class	\
  $(DIR)/Reply.class

$(DIR)/Environment.class:  $(DIR)/BuiltInFunctions.class $(DIR)/EvaluationTypeEnv.class
# $(DIR)/EvaluationTypeChecker.class	\

$(DIR)/BinOpApply.class: $(DIR)/Value.class

$(DIR)/Execution.class: $(DIR)/BinOpApply.class $(DIR)/Statement.class $(DIR)/Environment.class

# Type checking

$(DIR)/Substitution.class: $(DIR)/FunctionType.class $(DIR)/Reply.class

$(DIR)/TypeEnv.class: $(DIR)/Exp.class $(DIR)/BuiltInFunctions.class	\
  $(DIR)/Substitution.class $(DIR)/EvaluationTypeEnv.class

$(DIR)/Unification.class: $(DIR)/TypeEnv.class	
#  $(DIR)/EvaluationTypeChecker.class

$(DIR)/TypeChecker.class: $(DIR)/Unification.class $(DIR)/Substitution.class	\
  $(DIR)/Exp.class $(DIR)/FunctionValue.class $(DIR)/Statement.class

# Parsing and tests

$(DIR)/Parser.class: $(DIR)/Input.class

$(DIR)/Parser0.class: $(DIR)/Parser.class $(DIR)/Exp.class

$(DIR)/DeclarationParser.class: $(DIR)/Parser0.class $(DIR)/Statement.class	\
  $(DIR)/TypeConstraint.class $(DIR)/FunctionValue.class

$(DIR)/StatementParser.class: $(DIR)/DeclarationParser.class

$(DIR)/ParserTest0.class:  $(DIR)/StatementParser.class $(DIR)/Execution.class

$(DIR)/ExpParserTest.class: $(DIR)/ParserTest0.class

$(DIR)/StatementParserTest.class: $(DIR)/ExpParserTest.class

$(DIR)/ParserTest.class: $(DIR)/StatementParserTest.class 

$(DIR)/TypeCheckerTest0.class: $(DIR)/TypeChecker.class	\
  $(DIR)/StatementParser.class

$(DIR)/TypeCheckerTest1.class $(DIR)/TypeCheckerTest2.class $(DIR)/TypeCheckerTest3.class: $(DIR)/TypeCheckerTest0.class

$(DIR)/TypeCheckerTest.class: $(DIR)/TypeCheckerTest1.class $(DIR)/TypeCheckerTest2.class $(DIR)/TypeCheckerTest3.class

# ===== Model

$(DIR)/Model.class: $(DIR)/ViewT.class $(DIR)/StatementParser.class	\
  $(DIR)/TypeChecker.class $(DIR)/Execution.class

# ===== View

$(DIR)/Spreadsheet.class: $(DIR)/ViewT.class $(DIR)/Model.class

$(DIR)/View.class: $(DIR)/Model.class $(DIR)/Spreadsheet.class $(DIR)/ViewT.class

# ===== Top level

$(DIR)/TopLevelTest.class: $(DIR)/Model.class

$(DIR)/AllTests.class: $(DIR)/ParserTest.class $(DIR)/TypeCheckerTest.class $(DIR)/TopLevelTest.class

$(DIR)/SpreadsheetApp.class: $(DIR)/Spreadsheet.class $(DIR)/View.class

clean:
	rm $(DIR)/*.class *.class; fsc -shutdown

$(DIR)/%.class:     %.scala
	fsc -deprecation -cp $(CP) $<
