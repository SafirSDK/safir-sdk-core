###################################################################################################
#
# Makefile
#
# Use "make help" or look under the help target to see the targets that users should use.
#  (there are a lot of other targets that are mostly for internal use)
#
#   This file is supposed to be placed in the same directory as the .dou-files
#   (xml files containing unit descriptions), and Makefile.conf. Makefile.conf
#   contains the configurable part of the makefile. 
#
# Dependencies:
#   gnumake, but not msys/mingw since windows paths are used.
#
# 041220/HESU: Created.
# 050208/HESU: Adapted to the tower structure (the kernel directory is still in use though)
# 050303/HESU: Removed the kernel directory.
# 050315/HESU: Made the compilation continue after error (since dotnet isn't functional)
# 050405/HESU: Added deletion of all previous files before install. Note that any misplaced
#              h-file will be deleted from the include directory. Removed the copying of the
#              dotnet files, since there is a warning, due to the fact that we currently do
#              not build them.
# 050406/HESU: Added dotnet again.
# 050410/HESU: Added preliminary support for java.
# 050512/HESU: Added usage of Strong Name Key for C#.
# 050516/HESU: Added compilation and jar-creation/installation for Java
# 050517/HESU: Made it possible to run without Java.
# 050519/HESU: Corrected the root path in the produced jar-file.
# 050524/HESU: Relaxed the test for JAVA_HOME, to be able to handle definitions containing space.
# 050610/HESU: Added the C#-dlls to the GAC.
# 050905/HESU: Removed installation of the C#-dlls in the GAC. Remove previous dlls from your GAC manually!
# 050913/HESU: Changed the cpp-linkage to avoid too long command line.
#              Removed the debug version for dotnet.
# 051106/HESU: Added tag_dir. A dod-file for info about the supported tags in the templates
#              parser is provided. These files can make it easier to implement dod-files.
# 051108/HESU: Added target check, which is built after install.
# 051123/STLRHA: Rewrite to get better performance, the cpp build has been moved into Makefile_cpp
#                This makefile may still have problems with lots of files. Regard it as BETA quality.
# 060323/ASEK  A temporary fix when using Visual Studio 2005: 
#              Removed the flag "Treat warning as errors" when building "dots_generated-dotnet.dll".
#              The new warning appears in "AssemblyInfo.cs".
#              Also changed copying of files : All *.dll-files, and *.jar-files shall be copied 
#              to the folder $(SAFIR_RUNTIME)\bin
# 2006-05-12 STLRHA : Changed to use make_vs_setup include file
#		      Fixed Java building to work with many dou files
#                     Fixed the warning described above and readded /warnaserror to dotnet build.
# 2006-06-02 HESU   : Adapted to hierarchical include structure.
# 2006-06-02 HESU/STLRHA: Improved. But install runs for every make anyway.
# 2008-09-02 HESU: Removed creation of Ada/Java.
#
###################################################################################################

.SILENT:

# Makefile.conf is only used to override the defaults.
# It is only here for the convenience of the developers.
-include Makefile.conf

ifeq ($(strip $(dod_dir)),)
dod_dir = $(SAFIR_RUNTIME)\data\text\dots\config
endif

ifeq ($(strip $(dots_v)),)
dots_v  = $(SAFIR_RUNTIME)\bin\dots_v
endif

include $(SAFIR_SDK)\\data\\build\\make_vs_setup

GNATMAKE=gnatmake.exe
#JAR_DIR := $(SAFIR_RUNTIME)\bin
ASSEMBLY_DIR := %SAFIR_RUNTIME%\bin
SDK_DIR := %SAFIR_SDK%

TAG_DIR := tags

CPP_DIR := cpp
CPP_INCLUDE_DIR := $(CPP_DIR)\include
CPP_DEBUG_DIR := $(CPP_DIR)\debug
CPP_RELEASE_DIR := $(CPP_DIR)\release

DOTNET_DIR := dotnet

#ADA_DOTS_GENERATED_DIR := $(SAFIR_SDK)\ada\dots_generated-ada $(SAFIR_SDK)\ada\dots_generated-ada\lib-obj $(SAFIR_SDK)\ada\dots_generated-ada\lib-obj\debug $(SAFIR_SDK)\ada\dots_generated-ada\lib-obj\release
ADA_DIR := ada
#ADA_DIRS := $(ADA_DIR) $(ADA_DIR)\lib-obj $(ADA_DIR)\lib-obj\debug $(ADA_DIR)\lib-obj\release $(ADA_DIR)\lib-obj\release-lib $(ADA_DIR)\lib-obj\debug-lib  
JAVA_DIR := java
#JAVA_SRC_DIR := $(JAVA_DIR)\src
#JAVA_BIN_DIR := $(JAVA_DIR)\bin

UNUSED_STUFF_TO_DELETE := dep

SOURCES := $(wildcard *.dou)
DOM_FILES := $(wildcard *.dom)

DOD_FILES := $(wildcard $(dod_dir)/*.dod)


vpath %.cpp $(CPP_DIR)
#vpath %.ada $(ADA_DIR)
vpath %.cs $(DOTNET_DIR)
#vpath %.jar $(JAVA_DIR)
##vpath %.java $(JAVA_SRC_DIR)
vpath %.dll $(DOTNET_DIR) $(CPP_DIR)\\debug $(CPP_DIR)\\release

default: dirs build install_flag check

#%.cpp %.adb %.ads %.cs : %.dou $(DOD_FILES)
%.cpp %.cs : %.dou $(DOD_FILES)
	$(dots_v) -dod=$(dod_dir) $< 

#relies on the fact that dots_v always generates .cpp files
.PHONY: generate
generate: dirs $(SOURCES:%.dou=%.cpp)


#.PHONY: dots_generated-cppd.dll
dots_generated-cppd.dll: $(SOURCES:%.dou=%.cpp)
	@$(MAKE) -f Makefile_cpp TARGET="debug" $@

#.PHONY: dots_generated-cpp.dll
dots_generated-cpp.dll: $(SOURCES:%.dou=%.cpp)
	@$(MAKE) -f Makefile_cpp TARGET="release" $@

##.PHONY: dots_generated-adad.dll
#dots_generated-adad.dll: $(SOURCES:%.dou=%.adb)
#	${GNATMAKE} -Pdots_generated_library.gpr -XBUILD=debug -XWarning_Level=Default

##.PHONY: dots_generated-ada.dll
#dots_generated-ada.dll: $(SOURCES:%.dou=%.adb)
#	${GNATMAKE} -Pdots_generated_library.gpr -XBUILD=release -XWarning_Level=Default


.PHONY: build
build: dots_generated-cppd.dll dots_generated-cpp.dll dots_generated-dotnet.dll
#ifneq ($(strip $(JAVA_HOME)),)  #This bit adds the jar file as a prerequisite to build if JAVA_HOME is defined. Otherwise a message is printed
#build: dots_generated-java.jar
#else
#	echo *** JAVA_HOME is not defined. NO JAR-file was produced.
#endif
#ifneq ($(strip $(ADA_PROJECT_PATH)),)  #Build the ada code if ADA_PROJECT_PATH is defined. Otherwise a message is printed
#build: dots_generated-ada.dll dots_generated-adad.dll
#else
#	echo *** ADA_PROJECT_PATH is not defined. NO ADA code was built.
#endif


.PHONY: dirs
dirs:
	echo Creating directories
#	-mkdir $(TAG_DIR) $(ADA_DIRS) $(ADA_DOTS_GENERATED_DIR) $(DOTNET_DIR) $(CPP_DIR) $(CPP_INCLUDE_DIR) $(CPP_RELEASE_DIR) $(CPP_DEBUG_DIR) $(JAVA_DIR) $(JAVA_SRC_DIR) $(JAVA_BIN_DIR) >\nul 2>\nul
	-mkdir $(TAG_DIR) $(DOTNET_DIR) $(CPP_DIR) $(CPP_INCLUDE_DIR) $(CPP_RELEASE_DIR) $(CPP_DEBUG_DIR) >\nul 2>\nul

dots_generated-dotnet.dll : $(SOURCES:%.dou=%.cs)
	echo Building (.Net): $@
	copy dots_generated-dotnet.snk $(DOTNET_DIR) >\nul
	$(NET_SETUP) >\nul && cd $(DOTNET_DIR) && \
		csc /warnaserror /nologo /target:library /out:$@ /lib:$(ASSEMBLY_DIR) /reference:Safir.Dob.TypeSystem.dll /keyfile:dots_generated-dotnet.snk *.cs ..\AssemblyInfo.cs 

##relies on the fact that dots_v always generates .cpp files
#java_build.txt: $(SOURCES:%.dou=%.cpp)
#	echo Creating java build help file
#	dots_java_build_helper .

##relies on the fact that dots_v always generates .cpp files
#dots_generated-java.jar: $(SOURCES:%.dou=%.cpp) java_build.txt
#	echo Building $@
#	javac -cp "$(JAVA_SRC_DIR);$(JAR_DIR)\safir.dob.typesystem.jar" -d $(JAVA_BIN_DIR) @java_build.txt
#	jar cvf $(JAVA_DIR)\$@ -C $(JAVA_BIN_DIR) . >\nul

.PHONY: install
install: build install_flag

install_flag : build
	echo Installing
	-del /Q $(SAFIR_RUNTIME)\data\text\dots\classes\*.*
#ifneq ($(strip $(ADA_PROJECT_PATH)),)
#	-copy $(SAFIR_SDK)\dots\dots_generated\ada\*.ads $(SAFIR_SDK)\ada\dots_generated-ada >\nul
#	-copy $(SAFIR_SDK)\dots\dots_generated\ada\lib-obj\release-lib\libdots_generated-ada.dll $(SAFIR_RUNTIME)\bin >\nul
#	-copy $(SAFIR_SDK)\dots\dots_generated\ada\lib-obj\debug-lib\libdots_generated-adad.dll $(SAFIR_RUNTIME)\bin >\nul
#	-copy $(SAFIR_SDK)\dots\dots_generated\ada\lib-obj\release-lib\*.ali $(SAFIR_SDK)\ada\dots_generated-ada\lib-obj\release >\nul
#	-copy $(SAFIR_SDK)\dots\dots_generated\ada\lib-obj\debug-lib\*.ali $(SAFIR_SDK)\ada\dots_generated-ada\lib-obj\debug >\nul
#endif
	-copy $(CPP_DEBUG_DIR)\*.dll %SAFIR_RUNTIME%\bin >\nul
	-copy $(CPP_RELEASE_DIR)\*.dll %SAFIR_RUNTIME%\bin >\nul
	-copy $(CPP_DEBUG_DIR)\*.lib %SAFIR_SDK%\lib >\nul
	-copy $(CPP_RELEASE_DIR)\*.lib %SAFIR_SDK%\lib >\nul
	-copy $(DOTNET_DIR)\*.dll $(ASSEMBLY_DIR) >\nul
	-xcopy $(CPP_INCLUDE_DIR) $(SAFIR_SDK)\include /E /Y >\nul
	-copy *.dou $(SAFIR_RUNTIME)\data\text\dots\classes >\nul
	-copy *.dom  $(SAFIR_RUNTIME)\data\text\dots\classes >\nul 2>\nul
#ifneq ($(strip $(JAVA_HOME)),)  #This bit adds the jar file as a prerequisite to build if JAVA_HOME is defined. Otherwise a message is printed
#	-copy $(JAVA_DIR)\*.jar $(SAFIR_RUNTIME)\bin >\nul 2>\nul
#endif
	touch install_flag

check:
	dots_configuration_check


validation_flag: $(SOURCES)
	dots_schema_validator .
	touch validation_flag

clean:
	-rm -f -r $(ADA_DIR) $(CPP_DIR) $(DOTNET_DIR) $(TAG_DIR) $(JAVA_DIR) $(UNUSED_STUFF_TO_DELETE) install_flag java_build.txt validation_flag

my: rebuild

day: install_flag check
	echo Did it make you feel lucky, Punk?

love: 
	@echo You make sweet tender love to the DOB, and the DOB loves you back!

generate_rebuild: validation_flag dirs
	$(dots_v) -dod=$(dod_dir) *.dou 


rebuild: clean generate_rebuild build

help: 
	@echo These targets are defined:
	@echo   default  : (default target) same as dirs generate build install check
	@echo   dirs     : creates directories for the generated files
	@echo   generate : generates source code from the dou files.
	@echo   build    : compiles/links the generated source code
	@echo                with the tools appropriate for each language.
	@echo   rebuild  : does the same as 'clean build', but slightly faster
	@echo   install  : copies the result to the target directories.
	@echo   check    : checks that the installed result is correct.
	@echo   clean    : removes all generated files and directories.