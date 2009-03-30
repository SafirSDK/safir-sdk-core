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
# 060510/HESU: Added VS_SETUP for VS80COMNTOOLS.
#
###################################################################################################

.SILENT:

include $(SAFIR_SDK)\\data\\build\\make_vs_setup

all: dirs test

sources = $(wildcard DotsTestCpp/*.cpp)

dep1 = $(SAFIR_SDK)\lib\dots_kernel.lib
dep2 = $(SAFIR_SDK)\lib\dots_generated-cpp.lib

goal = ..\output\cpp.txt

dirs:
	-mkdir ..\output >\nul 2>\nul

DotsTestCpp.exe : $(sources) $(dep1) $(dep2)
	$(VC_SETUP) >\nul & cl /nologo /MD /GR /EHsc /I %SAFIR_SDK%\include DotsTestCpp\*.cpp /link /LIBPATH:%SAFIR_SDK%\lib 

$(goal) : DotsTestCpp.exe
	DotsTestCpp.exe >$(goal)

print : $(goal)
	type $(goal)

test : $(goal)

clean:
	-del $(goal) *.obj *.exe *.manifest >\nul 2>\nul


