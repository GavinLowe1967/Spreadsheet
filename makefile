CP = .:/home/gavin/Scala/lib/scala-swing_2.13-3.0.0.jar

DIR = spreadsheet

all: $(DIR)/ParserTest.class $(DIR)/SpreadsheetApp.class $(DIR)/TypeCheckerTest.class 

# ===== Language

$(DIR)/Value.class: $(DIR)/Input.class $(DIR)/TypeT.class

$(DIR)/Environment.class: $(DIR)/Value.class

$(DIR)/Exp.class: $(DIR)/Value.class $(DIR)/Environment.class

$(DIR)/FunctionValue.class: $(DIR)/Exp.class

$(DIR)/Statement.class: $(DIR)/Exp.class $(DIR)/ViewT.class

$(DIR)/BlockExp.class: $(DIR)/Statement.class

# Parsing

$(DIR)/Parser.class: $(DIR)/Input.class

$(DIR)/StatementParser.class: $(DIR)/Parser.class $(DIR)/FunctionValue.class	\
  $(DIR)/BlockExp.class

$(DIR)/ParserTest.class: $(DIR)/StatementParser.class

# Type checking

$(DIR)/TypeConstraint.class: $(DIR)/TypeT.class

$(DIR)/Substitution.class: $(DIR)/TypeT.class

$(DIR)/TypeEnv.class: $(DIR)/TypeConstraint.class

$(DIR)/TypeChecker.class: $(DIR)/Substitution.class				\
  $(DIR)/TypeEnv.class $(DIR)/Exp.class $(DIR)/FunctionValue.class	\
  $(DIR)/BlockExp.class

$(DIR)/TypeCheckerTest.class: $(DIR)/TypeChecker.class

# ===== Model

$(DIR)/Model.class: $(DIR)/ViewT.class $(DIR)/StatementParser.class

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
