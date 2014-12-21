PROJECT(safir-sdk-core CXX C)

#We need a very recent cmake on windows. The reason is that test property
#and install generator expressions are only supported in cmake 3.
if(MSVC)
  cmake_minimum_required(VERSION 3.0.0)
else()
  cmake_minimum_required(VERSION 2.8.12)
endif()

#Set up the Safir SDK Core version by parsing version.txt
#IMPORTANT: THE version.txt FILE NEEDS TO BE UPDATED FOR EVERY RELEASE
#Remember: any updates that change APIs should update MAJOR, since
#that is used for SOVERSION.
#Set the SUFFIX to empty string for stable releases
file(STRINGS VERSION.txt version_strings)
foreach (line IN LISTS version_strings)
  #skip comments and empty lines
  if (NOT line MATCHES "^#.*" AND NOT line STREQUAL "")
    string(REPLACE "=" ";" line ${line})
    list(GET line 0 key)
    list(GET line 1 value)
    if (key STREQUAL "MAJOR")
      set(SAFIR_VERSION_MAJOR "${value}")
    elseif(key STREQUAL "MINOR")
      set(SAFIR_VERSION_MINOR "${value}")
    elseif(key STREQUAL "PATCH")
      set(SAFIR_VERSION_PATCH "${value}")
    elseif(key STREQUAL "SUFFIX")
      set(SAFIR_VERSION_SUFFIX "${value}")
    else()
      message(FATAL_ERROR "Failed to parse version.txt file")
    endif()
  endif()
endforeach()

SET(SAFIR_VERSION_STRING "${SAFIR_VERSION_MAJOR}.${SAFIR_VERSION_MINOR}.${SAFIR_VERSION_PATCH}${SAFIR_VERSION_SUFFIX}")

#
# This function will install a python script into the bin directory.
# If it is a *nix system the script will have ".py" stripped from its name.
#
function (SAFIR_INSTALL_PYTHON)
  cmake_parse_arguments(INST "" "SCRIPT;COMPONENT;RENAME" "" ${ARGN})

  if (NOT "${INST_UNPARSED_ARGUMENTS}" STREQUAL "")
    message(FATAL_ERROR "Unknown argument to SAFIR_INSTALL_PYTHON '${INST_UNPARSED_ARGUMENTS}'")
  endif()

  if(INST_RENAME AND NOT MSVC)
    STRING(REGEX REPLACE "\\.py$" "" _stripped_name "${INST_RENAME}")
    GET_FILENAME_COMPONENT(_stripped_name ${_stripped_name} NAME)
    SET(_RENAME_ARG RENAME ${_stripped_name})
  elseif(INST_RENAME)
    SET(_RENAME_ARG RENAME ${INST_RENAME})
  elseif(NOT MSVC)
    STRING(REGEX REPLACE "\\.py$" "" _stripped_name "${INST_SCRIPT}")
    GET_FILENAME_COMPONENT(_stripped_name ${_stripped_name} NAME)
    SET(_RENAME_ARG RENAME ${_stripped_name})
  endif()

  INSTALL(PROGRAMS ${INST_SCRIPT}
    DESTINATION ${SAFIR_INSTALL_DESTINATION_BIN}
    COMPONENT ${INST_COMPONENT}
    ${_RENAME_ARG})
endfunction()


#set up install directories
SET(SAFIR_INSTALL_DESTINATION_INCLUDE include)
SET(SAFIR_INSTALL_DESTINATION_BIN bin)
SET(SAFIR_INSTALL_DESTINATION_LIB lib)

if (UNIX)
  SET(SAFIR_INSTALL_DESTINATION_CMAKE lib/cmake/SafirSDKCore)
  SET(SAFIR_INSTALL_DESTINATION_DOC share/doc/safir-sdk-core)
  SET(SAFIR_INSTALL_DESTINATION_MAN share/man)
  SET(SAFIR_INSTALL_DESTINATION_CSHARP lib/safir-sdk-core)
  SET(SAFIR_INSTALL_DESTINATION_JAR share/java/safir-sdk-core)
  SET(SAFIR_INSTALL_DESTINATION_TEST_DATA share/safir-sdk-core/test_data)
  SET(SAFIR_INSTALL_DESTINATION_GENERATION share/safir-sdk-core/generation)
  SET(SAFIR_INSTALL_DESTINATION_DOU_BASE share/safir-sdk-core/dou)
else()
  SET(SAFIR_INSTALL_DESTINATION_CMAKE cmake)
  SET(SAFIR_INSTALL_DESTINATION_DOC docs)
  SET(SAFIR_INSTALL_DESTINATION_CSHARP dotnet)
  SET(SAFIR_INSTALL_DESTINATION_JAR java)
  SET(SAFIR_INSTALL_DESTINATION_TEST_DATA test_data)
  SET(SAFIR_INSTALL_DESTINATION_GENERATION generation)
  SET(SAFIR_INSTALL_DESTINATION_DOU_BASE dou)
  SET(SAFIR_INSTALL_DESTINATION_INST_UTIL installer_utils)
endif()

SET(CMAKE_MODULE_PATH ${CMAKE_CURRENT_SOURCE_DIR}/src/cmake ${CMAKE_MODULE_PATH})

INCLUDE_DIRECTORIES(include)
INSTALL(DIRECTORY include/ DESTINATION ${SAFIR_INSTALL_DESTINATION_INCLUDE}
  COMPONENT Development
  PATTERN ".svn" EXCLUDE
  PATTERN "*~" EXCLUDE)

#TODO: create a version.h file and install it. Not sure how to do it best...

set(CTEST_FULL_OUTPUT 1)
ENABLE_TESTING()

add_subdirectory(src)
add_subdirectory(docs)

SAFIR_INSTALL_PYTHON(
  SCRIPT build/build.py
  RENAME dobmake-batch.py
  COMPONENT Development)

INSTALL(FILES COPYING.txt
  DESTINATION ${SAFIR_INSTALL_DESTINATION_DOC}
  RENAME LICENSE.txt
  COMPONENT Runtime)

#TODO: should dobexplorer be in dev package?
