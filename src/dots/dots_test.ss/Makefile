###################################################################################################
#
# Makefile
#
# These targets are defined:
#   all: (default)
#   print: 
#   clean: removes all generated files.
#
# 050809/HESU: Created
#
###################################################################################################
.PHONY: all print clean generate
.SILENT:

default: all

dirs:
	-mkdir new_output >\nul 2>\nul

all: dirs
#	-$(MAKE) -C ada all
	-$(MAKE) -C cpp all
	-$(MAKE) -C dotnet all
#	-$(MAKE) -C java all

print: dirs
#	$(MAKE) -C ada print
	$(MAKE) -C cpp print
	$(MAKE) -C dotnet print
#	$(MAKE) -C java print

clean:
#	$(MAKE) -C ada clean
	$(MAKE) -C cpp clean
	$(MAKE) -C dotnet clean
#	$(MAKE) -C java clean

generate:
	-xcopy /Y data\\*.dou $(SAFIR_SDK)\dots\dots_generated
	-xcopy /Y data\\*.dom $(SAFIR_SDK)\dots\dots_generated
	$(foreach file, $(patsubst data%,$(SAFIR_SDK)\dots\dots_generated%, $(DOB_FILES)), touch $(file))
	cd $(SAFIR_SDK)\dots\dots_generated && make