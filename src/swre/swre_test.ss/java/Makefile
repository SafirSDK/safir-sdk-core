###################################################################################################
#
# Makefile
#
# Used to build and run Ada test programs.
#
#
###################################################################################################
.PHONY: default build clean install dirs

.SILENT:

SOURCES = $(wildcard src/*.java)

CLASSPATH := $(SAFIR_RUNTIME)\bin\safir.dob.jar;$(SAFIR_RUNTIME)\bin\safir.dob.typesystem.jar;$(SAFIR_RUNTIME)\bin\dots_generated-java.jar;$(SAFIR_RUNTIME)\bin\safir.swreports.jar


default: build

build: dirs
	javac -sourcepath "src" -cp $(CLASSPATH) -d bin $(SOURCES)

jar: build
#	copy $(SAFIR_RUNTIME)\bin\safir.dob.jar + $(SAFIR_RUNTIME)\bin\safir.dob.typesystem.jar + $(SAFIR_RUNTIME)\bin\dots_generated-java.jar + $(SAFIR_RUNTIME)\bin\safir.swreports.jar .
	jar cmfv manifest.txt swre_test_java.jar -C "bin" .

install: jar
	copy swre_test_java.jar $(SAFIR_RUNTIME)\bin

dirs:
	-mkdir bin >\nul 2>\nul
clean:
	-rm -rf bin *.jar
	-rm -f $(SAFIR_RUNTIME)\bin\swre_test_java.jar 

#	-del /F /Q $(goal) bin\DoseTestJava\* flag >\nul 2>\nul

#this is used to get a rebuild when something has changed in dots
# DOTS_INSTALLED = $(SAFIR_SDK)\dots\dots_generated\install_flag

# TEST_OUTPUT_FILENAME = ..\new_output\java.txt

# MY_CLASSPATH = $(SAFIR_RUNTIME)\bin\safir.dob.jar;$(SAFIR_RUNTIME)\bin\safir.dob.typesystem.jar;$(SAFIR_RUNTIME)\bin\dots_generated-java.jar

# #the main class to run
# TARGET_EXECUTABLE_NAME = DoseTestJava.DoseTestJava

# default: build

# test: dirs build
# 	echo Testing
# 	java -cp "bin;$(MY_CLASSPATH)" $(TARGET_EXECUTABLE_NAME) >$(TEST_OUTPUT_FILENAME)
# 	echo Test complete!

# dirs:
# 	-mkdir bin >\nul 2>\nul
# 	-mkdir ..\new_output >\nul 2>\nul

# build: $(SOURCES) $(DOTS_INSTALLED)
# 	echo Building
# 	javac -classpath "src/DoseTestJava/;$(MY_CLASSPATH)" -d bin $(SOURCES)

# run : build
# 	echo Executing
# 	java -cp "bin;$(MY_CLASSPATH)" $(TARGET_EXECUTABLE_NAME)

# print: test
# 	type $(TEST_OUTPUT_FILENAME)

# clean:
# 	-del /F /Q $(goal) bin\DoseTestJava\* flag >\nul 2>\nul


