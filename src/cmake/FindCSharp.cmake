# - Try to find the a csharp compiler and associated tools
#
# defines
#
# CSHARP_FOUND - system has a csharp compiler and associated tools
# CSHARP_COMPILER - where to find 'gmcs' or 'csc.exe'
# GACUTIL_EXECUTABLE - where to find 'gacutil'
# RESGEN_EXECUTABLE - where to find 'resgen' or 'resgen2'
#
# On Win32 we also define
#   DOTNET_FRAMEWORK_LIBPATH - directory where used .NET Framework assemblies can be found.
#   CSHARP_COMPILER_FRAMEWORK_ARGUMENTS - compiler arguments needed to use the
#          assemblies found in DOTNET_FRAMEWORK_LIBPATH. These can go in a response file,
#          but you will need to specify -noconfig directly on the csc commandline.
#
# copyright (c) 2007 Arno Rehn arno@arnorehn.de
# copyright (c) 2008,2014-2016 Lars Hagstrom lars.hagstrom@consoden.se
#
# Redistribution and use is allowed according to the terms of the GPL license.


FIND_PROGRAM (CSHARP_COMPILER NAMES mcs csc cli-csc gmcs gmcs2)
FIND_PROGRAM (CSHARP_LINKER NAMES cli-al al)
FIND_PROGRAM (GACUTIL_EXECUTABLE gacutil)
FIND_PROGRAM (RESGEN_EXECUTABLE NAMES cli-resgen resgen2 resgen PATH_SUFFIXES ..) #in vs2013 express x64 we need to look one step up!
FIND_PROGRAM (SN_EXECUTABLE NAMES sn)

SET (CSHARP_FOUND FALSE)

IF (CSHARP_COMPILER AND GACUTIL_EXECUTABLE AND RESGEN_EXECUTABLE AND CSHARP_LINKER AND SN_EXECUTABLE)
    SET (CSHARP_FOUND TRUE)
ENDIF ()

#Check if we're using MONO.
EXECUTE_PROCESS(COMMAND ${CSHARP_COMPILER} --version
  OUTPUT_VARIABLE version_output)
if (version_output MATCHES "Mono")
    SET(CSHARP_IS_MONO TRUE)
endif()
SET(version_output "")

IF (NOT CSharp_FIND_QUIETLY)
  MESSAGE(STATUS "Found CSharp compiler: ${CSHARP_COMPILER}")
  MESSAGE(STATUS "Found CSharp linker: ${CSHARP_LINKER}")
  MESSAGE(STATUS "Found gacutil: ${GACUTIL_EXECUTABLE}")
  MESSAGE(STATUS "Found resgen: ${RESGEN_EXECUTABLE}")
  MESSAGE(STATUS "Found sn: ${SN_EXECUTABLE}")
ENDIF ()

IF (NOT CSHARP_FOUND)
  IF (CSharp_FIND_REQUIRED)
    MESSAGE(FATAL_ERROR "Could not find one or more of the C# development tools")
  ENDIF ()
ENDIF ()


# On Windows we want to ensure that we target version 4.8 of the .NET framework, falling back on 4.6.1 for
# vs2015 compatibility, so we point it out specifically. On Linux it doesnt matter so much, since we
# target whateveris in  the distro repos.
if (WIN32)
  set (_DOTNET_FRAMEWORK_VERSIONS v4.8 v4.6.1)
  foreach (ver ${_DOTNET_FRAMEWORK_VERSIONS})
    set (DOTNET_FRAMEWORK_LIBPATH "C:/Program Files (x86)/Reference Assemblies/Microsoft/Framework/.NETFramework/${ver}/")
    if (NOT IS_DIRECTORY "${DOTNET_FRAMEWORK_LIBPATH}")
      set (DOTNET_FRAMEWORK_LIBPATH "C:/Program Files/Reference Assemblies/Microsoft/Framework/.NETFramework/${ver}/")
    endif()
    if (IS_DIRECTORY "${DOTNET_FRAMEWORK_LIBPATH}")
      message(STATUS "Using .NET Framework assemblies in ${DOTNET_FRAMEWORK_LIBPATH}")
      break()
    else()
      unset(DOTNET_FRAMEWORK_LIBPATH)
    endif()
  endforeach()
  unset(ver)
  unset(_DOTNET_FRAMEWORK_VERSIONS)

  if (NOT DOTNET_FRAMEWORK_LIBPATH)
    message(FATAL_ERROR "Could not find the .NET assemblies")
  endif()

  SET(CSHARP_COMPILER_FRAMEWORK_ARGUMENTS "-nostdlib
                              -lib:\"${DOTNET_FRAMEWORK_LIBPATH}\"
                              -reference:\"${DOTNET_FRAMEWORK_LIBPATH}mscorlib.dll\"
                              -reference:\"${DOTNET_FRAMEWORK_LIBPATH}System.dll\"
                              -reference:\"${DOTNET_FRAMEWORK_LIBPATH}Microsoft.CSharp.dll\"
                              -reference:\"${DOTNET_FRAMEWORK_LIBPATH}System.Configuration.dll\"
                              -reference:\"${DOTNET_FRAMEWORK_LIBPATH}System.Core.dll\"
                              -reference:\"${DOTNET_FRAMEWORK_LIBPATH}System.Xml.dll\"
                              -reference:\"${DOTNET_FRAMEWORK_LIBPATH}System.EnterpriseServices.dll\"")
endif()

MARK_AS_ADVANCED(CSHARP_COMPILER CSHARP_LINKER GACUTIL_EXECUTABLE RESGEN_EXECUTABLE DOTNET_FRAMEWORK_LIBPATH CSHARP_COMPILER_FRAMEWORK_ARGUMENTS)
