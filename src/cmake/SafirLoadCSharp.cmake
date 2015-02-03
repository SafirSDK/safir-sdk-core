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
    SET(CSHARP_COMPILER_FLAGS "-warn:4 -nowarn:1607")

    #Get platform and convert it to lowercase (vs2010 has it as X64 and vs2013 express as x64!)
    string(TOLOWER "$ENV{Platform}" PLATFORM)

    #make sure we set the arch of dotnet assemblies to be the same as the native code we build.
    if (PLATFORM STREQUAL "x64")
      SET(CSHARP_COMPILER_FLAGS "${CSHARP_COMPILER_FLAGS} -platform:x64")
    else()
      SET(CSHARP_COMPILER_FLAGS "${CSHARP_COMPILER_FLAGS} -platform:x86")
    endif()
else()
    SET(CSHARP_COMPILER_FLAGS "-warn:4")
endif()
