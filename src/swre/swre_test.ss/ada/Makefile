###################################################################################################
#
# Makefile
#
# Used to build and run Ada test programs.
#
# 050909/HESU: Created
# 051006/HESU: Restructured according to the makefile in the directory above.
# 070113/LAHA: Rewrite
#
###################################################################################################
.PHONY: default build clean install dirs

.SILENT:

default: build

build: dirs
	gnatmake -Pswre_test_ada.gpr swre_test_ada.adb -d -XBUILD=release -XWarning_Level=Default >\nul

install: build
	copy swre_test_ada.exe $(SAFIR_RUNTIME)\bin

dirs:
	-mkdir lib-obj lib-obj\debug lib-obj\release >\nul 2>\nul

clean:
	-rm -rf lib-obj *.exe
	-rm -f $(SAFIR_RUNTIME)\bin\swre_test_ada.exe

