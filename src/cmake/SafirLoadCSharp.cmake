#
# Check for existence of CSharp compiler and set up compilation flags for Safir build tree.
#
# Defines CSHARP_FOUND if a csharp compiler could be found
#

if (SAFIR_SDK_CORE_INSTALL_DIR)
  list(APPEND CMAKE_MODULE_PATH ${SAFIR_SDK_CORE_CMAKE_DIR})
endif()

set(CSharp_FIND_QUIETLY True)
find_package(CSharp)
if (NOT CSHARP_FOUND)
  MESSAGE(STATUS "Failed to find the C# development tools, will not build .NET interfaces")
else()
  INCLUDE(CSharpMacros)
endif()

if (MSVC)
  if (CMAKE_CONFIGURATION_TYPES AND CMAKE_BUILD_TYPE)
    MESSAGE(FATAL_ERROR "Both CMAKE_CONFIGURATION_TYPES and CMAKE_BUILD_TYPE are set! I can't work with this!")
  endif()

  SET(CSHARP_COMPILER_FLAGS "-warn:4 -nowarn:1607")
  #if we're in an IDE we get the platform from the generator
  if (CMAKE_CONFIGURATION_TYPES)
    if (CMAKE_GENERATOR MATCHES "Win64")
      set(PLATFORM "x64")
    endif()
  else()
    #Get platform and convert it to lowercase (vs2010 has it as X64 and vs2013 express as x64!)
    string(TOLOWER "$ENV{Platform}" PLATFORM)
  endif()

  #make sure we set the arch of dotnet assemblies to be the same as the native code we build.
  if (PLATFORM STREQUAL "x64")
    SET(CSHARP_COMPILER_FLAGS "${CSHARP_COMPILER_FLAGS} -platform:x64")
  else()
    SET(CSHARP_COMPILER_FLAGS "${CSHARP_COMPILER_FLAGS} -platform:x86")
  endif()
else()
  SET(CSHARP_COMPILER_FLAGS "-warn:4")

  #If we're compiling with Mono on ARM we need a newish version, since
  #older versions of mono do not work well on ARM. Here we test for 4.0
  #or later, but it is quite likely we would work with 3.4 or later.
  #We know we don't work with 3.2.8 and earlier.
  if (CSHARP_IS_MONO)
    FIND_PROGRAM (MONO_RUNTIME NAMES mono)
    mark_as_advanced(MONO_RUNTIME)
    if (NOT MONO_RUNTIME)
      MESSAGE(FATAL_ERROR "Could not find mono executable")
    endif()
    EXECUTE_PROCESS(COMMAND ${MONO_RUNTIME} --version
      OUTPUT_VARIABLE version_output)
    STRING(REGEX MATCH "Mono JIT compiler version [0-9]+\\.[0-9]+\\.[0-9]+ \\(" version ${version_output})
    STRING(REPLACE "Mono JIT compiler version " "" version ${version})
    STRING(REPLACE " (" "" version ${version})
    if (version VERSION_LESS "4.0" AND ${version_output} MATCHES "Architecture:.*armel")
      SET(CSHARP_FOUND FALSE)
      message(STATUS "The installed version of Mono is too old, need at least 4.0 on ARM. Will not build .NET interfaces!")
    endif()
    UNSET(version_output)
    UNSET(version)
    UNSET(MONO_RUNTIME)
  endif()
endif()
