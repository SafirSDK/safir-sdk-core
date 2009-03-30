###################################################################################################
#
# Makefile
#
# These targets are defined:
#	all (default): Compiles and runs the test
#	print: Displays the test result on the screen.
#	clean: removes all generated files.
#
# 050909/HESU: Created
#
###################################################################################################

.SILENT:


include $(SAFIR_SDK)\\data\\build\\make_vs_setup


assembly_dir = $(SAFIR_RUNTIME)\bin
sources = $(wildcard DotsTestDotnet/*.cs)

all: dirs test

dep1 = $(assembly_dir)\Safir.Dob.Typesystem.dll
dep2 = $(assembly_dir)\dots_generated-dotnet.dll
dep = $(dep1) $(dep2)

goal = ..\output\cs.txt

dirs:
	-mkdir ..\output >\nul 2>\nul
	copy $(dep1) . >\nul 2>\nul
	copy $(dep2) . >\nul 2>\nul

DotsTestDotnet.exe : $(sources) $(dep1) $(dep2) dirs
	$(NET_SETUP) >\nul & csc /debug:pdbonly /warnaserror /nologo /out:DotsTestDotnet.exe /lib:$(assembly_dir) /reference:$(dep1) /reference:$(dep2) DotsTestDotnet\*.cs

$(goal) : DotsTestDotnet.exe
	DotsTestDotnet.exe >$(goal)

print : $(goal)
	type $(goal)

test : $(goal)

clean:
	-del $(goal) *.dll *.exe *.pdb >\nul 2>\nul


