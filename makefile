CP = .:/home/gavin/Scala/lib/scala-swing_2.13-3.0.0.jar

DIR = spreadsheet

all: $(DIR)/StatementParser.class $(DIR)/SpreadsheetApp.class

# ===== Language

$(DIR)/Value.class: $(DIR)/Input.class

$(DIR)/Environment.class: $(DIR)/Value.class

$(DIR)/Exp.class: $(DIR)/Value.class $(DIR)/Environment.class

$(DIR)/Parser.class: $(DIR)/Input.class

$(DIR)/ExpParser.class: $(DIR)/Parser.class $(DIR)/Exp.class

$(DIR)/Statement.class: $(DIR)/Exp.class

$(DIR)/StatementParser.class: $(DIR)/ExpParser.class $(DIR)/Statement.class

# ===== Model

# $(DIR)/Cell.class: $(DIR)/Value.class

$(DIR)/Model.class: $(DIR)/ViewT.class $(DIR)/StatementParser.class

# ===== View

$(DIR)/Spreadsheet.class: $(DIR)/ExpParser.class $(DIR)/ViewT.class $(DIR)/Model.class 

$(DIR)/View.class: $(DIR)/Model.class $(DIR)/Spreadsheet.class $(DIR)/ViewT.class

# ===== Top level

$(DIR)/SpreadsheetApp.class: $(DIR)/Spreadsheet.class $(DIR)/View.class

clean:
	rm $(DIR)/*.class *.class; fsc -shutdown

$(DIR)/%.class:     %.scala
	fsc -deprecation -cp $(CP) $<
