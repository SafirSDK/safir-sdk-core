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


# ---------------------------------------------------------------------------
# Translate a framework version (e.g. 4.8 or 4.6.1) to the corresponding
# NuGet Target Framework Moniker (TFM).  Input is assumed to be only the
# numeric version, without any leading 'v' or trailing '-api'.
#
#   dotnet_version_to_tfm(<VER> <OUT_VAR>)
#
# Rule:
#   – Any version >= 4.8 maps to net481 (NuGet’s preferred moniker).
#   – Otherwise:  <major>.<minor>[.<patch>] → net<major><minor>[patch]
# ---------------------------------------------------------------------------
function(dotnet_version_to_tfm VER OUT_VAR)
  string(REPLACE "." ";" _parts "${VER}")
  list(LENGTH _parts _len)
  if(_len LESS 2)
    message(FATAL_ERROR "dotnet_version_to_tfm expects at least <major>.<minor>")
  endif()

  list(GET _parts 0 _major)
  list(GET _parts 1 _minor)

  if(_major EQUAL 4 AND _minor GREATER_EQUAL 8)
    set(_tfm "net481")
  else()
    set(_tfm "net${_major}${_minor}")
    if(_len GREATER 2)
      list(GET _parts 2 _patch)
      if(NOT _patch STREQUAL "0")
        set(_tfm "${_tfm}${_patch}")
      endif()
    endif()
  endif()

  set(${OUT_VAR} "${_tfm}" PARENT_SCOPE)
endfunction()

# ---------------------------------------------------------------------------
# Helper that resolves the location of reference assemblies for a sequence of
# requested framework versions.  The first version that exists on the local
# computer wins and its directory is returned in the caller-scope variable
# passed as the first argument.
#
#   find_dotnet_framework_libpath(<OUT_VAR> ver1 ver2 …)
#
# Works on both Windows (.NET reference assemblies) and Linux/Mono.
# ---------------------------------------------------------------------------
function(find_dotnet_framework_libpath OUT_VAR)
  set(_versions ${ARGN})

  foreach(_ver ${_versions})
    if (WIN32)
      # On Windows the framework directories are prefixed with 'v' (e.g. v4.8.1).
      # Add the prefix automatically so callers can pass plain version numbers
      set(_ver_dir "v${_ver}")
      set(_candidate_paths
          "C:/Program Files (x86)/Reference Assemblies/Microsoft/Framework/.NETFramework/${_ver_dir}/"
          "C:/Program Files/Reference Assemblies/Microsoft/Framework/.NETFramework/${_ver_dir}/")
    else()
      # Try pkg-config to locate Mono’s libdir
      execute_process(
        COMMAND pkg-config --variable=libdir mono
        OUTPUT_VARIABLE _mono_libdir
        OUTPUT_STRIP_TRAILING_WHITESPACE
        ERROR_QUIET)
      if (_mono_libdir)
        list(APPEND _candidate_paths
            "${_mono_libdir}/mono/${_ver}/"
            "${_mono_libdir}/mono/${_ver}/api/"
            "${_mono_libdir}/mono/${_ver}-api/")
      endif()
      # Fallback standard locations
      list(APPEND _candidate_paths
          "/usr/lib/mono/${_ver}/"
          "/usr/lib/mono/${_ver}/api/"
          "/usr/lib/mono/${_ver}-api/")
    endif()

    foreach(_p ${_candidate_paths})
      if (IS_DIRECTORY "${_p}")
        set(${OUT_VAR} "${_p}" PARENT_SCOPE)

        # Derive and expose the NuGet TFM
        dotnet_version_to_tfm("${_ver}" _derived_tfm)
        set(DOTNET_FRAMEWORK_TFM "${_derived_tfm}" CACHE STRING
            "NuGet Target Framework Moniker derived by FindCSharp")

        return()
      endif()
    endforeach()
    unset(_candidate_paths)
  endforeach()
endfunction()

find_dotnet_framework_libpath(DOTNET_FRAMEWORK_LIBPATH 4.8 4.6.1)

if (DOTNET_FRAMEWORK_LIBPATH)
  message(STATUS "Using .NET Framework assemblies in ${DOTNET_FRAMEWORK_LIBPATH}")
  message(STATUS "Using TFM ${DOTNET_FRAMEWORK_TFM}")
else()
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


MARK_AS_ADVANCED(CSHARP_COMPILER CSHARP_LINKER GACUTIL_EXECUTABLE RESGEN_EXECUTABLE DOTNET_FRAMEWORK_LIBPATH CSHARP_COMPILER_FRAMEWORK_ARGUMENTS DOTNET_FRAMEWORK_TFM)
