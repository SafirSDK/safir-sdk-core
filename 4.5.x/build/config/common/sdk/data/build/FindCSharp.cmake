# - Try to find the a csharp compiler and associated tools
#
# defines
#
# CSHARP_FOUND - system has a csharp compiler and associated tools
# CSHARP_COMPILER - where to find 'gmcs' or 'csc.exe'
# GACUTIL_EXECUTABLE - where to find 'gacutil'
# RESGEN_EXECUTABLE - where to find 'resgen' or 'resgen2'
#
# copyright (c) 2007 Arno Rehn arno@arnorehn.de
# copyright (c) 2008 Lars Hagstrom lars.hagstrom@saabgroup.com
#
# Redistribution and use is allowed according to the terms of the GPL license.

FIND_PROGRAM (CSHARP_COMPILER NAMES csc gmcs gmcs2)
FIND_PROGRAM (CSHARP_LINKER al)
FIND_PROGRAM (GACUTIL_EXECUTABLE gacutil)
FIND_PROGRAM (RESGEN_EXECUTABLE NAMES resgen2 resgen)

SET (CSHARP_FOUND FALSE)

IF (CSHARP_COMPILER AND GACUTIL_EXECUTABLE AND RESGEN_EXECUTABLE AND CSHARP_LINKER)
	SET (CSHARP_FOUND TRUE)
ENDIF ()

#Check if we're using MONO.
EXECUTE_PROCESS(COMMAND ${CSHARP_COMPILER} --version
  OUTPUT_VARIABLE version_output)
if (version_output MATCHES "Mono")
    SET(CSHARP_IS_MONO TRUE)
endif()
SET(version_output "")

if (CSHARP_IS_MONO)
    SET(DEBUG_INFO_FILE_SUFFIX ".mdb")
else()
    SET(DEBUG_INFO_FILE_SUFFIX ".pdb")
endif()


IF (CSHARP_FOUND)
	IF (NOT CSharp_FIND_QUIETLY)
		MESSAGE(STATUS "Found CSharp compiler: ${CSHARP_COMPILER}")
		MESSAGE(STATUS "Found CSharp linker: ${CSHARP_LINKER}")
		MESSAGE(STATUS "Found gacutil: ${GACUTIL_EXECUTABLE}")
		MESSAGE(STATUS "Found resgen: ${RESGEN_EXECUTABLE}")
	ENDIF ()
ELSE ()
	IF (CSharp_FIND_REQUIRED)
		MESSAGE(FATAL_ERROR "Could not find one or more of the following programs: (gmcs or csc), gacutil or (resgen or resgen2)")
	ENDIF ()
ENDIF ()

MARK_AS_ADVANCED(CSHARP_COMPILER GACUTIL_EXECUTABLE RESGEN_EXECUTABLE)
