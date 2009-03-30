###################################################################################################
#
# Makefile
#
# These targets are defined:
#   all: (default) same as build install.
#   build: compiles and creates a jar file.
#   install: copies the jar-file to the target directory.
#   clean: removes all generated files.
#
# 050519/HESU: Created
# 060426/HESU: Changed from jar to bin-dir in $(SAFIR_RUNTIME)
#
###################################################################################################

.SILENT:

all: build install

sources := $(wildcard src/safir/swreports/*.java) $(wildcard src/safir/application/*.java) 

goal = safir.swreports.jar
igoal = $(SAFIR_RUNTIME)\bin\$(goal)

CLASSPATH = "$(SAFIR_RUNTIME)\bin\safir.dob.jar;$(SAFIR_RUNTIME)\bin\safir.dob.typesystem.jar;$(SAFIR_RUNTIME)\bin\dots_generated-java.jar"
dirs:
	-mkdir bin >\nul 2>\nul

build: dirs $(goal)

$(goal) : $(sources)
	javac -classpath $(CLASSPATH) -d bin $(sources)
	jar cvf $(goal) -C bin . >\nul

install: $(igoal)

$(igoal) : $(goal)
	copy $(goal) $(igoal) >\nul

clean:
	-rm -f -r $(goal) bin/*

