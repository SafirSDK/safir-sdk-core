
PROJECT(safir_sdk_core CXX C)

#We need a very recent cmake on windows. The reason is that test property
#and install generator expressions are only supported in cmake 3.
if(MSVC)
  cmake_minimum_required(VERSION 3.0.0)
else()
  cmake_minimum_required(VERSION 2.8.12)
endif()

#Set up the Safir SDK Core version
#IMPORTANT: THESE NEED TO BE CHANGED FOR EVERY RELEASE!
#Remember: any updates that change APIs should update MAJOR, since
#that is used for SOVERSION.
SET(SAFIR_VERSION_MAJOR 6)
SET(SAFIR_VERSION_MINOR 0)
SET(SAFIR_VERSION_PATCH 0)
SET(SAFIR_VERSION_STRING "${SAFIR_VERSION_MAJOR}.${SAFIR_VERSION_MINOR}.${SAFIR_VERSION_PATCH}")

#TODO: !!
set(SAFIR_SDK "STOP_USING_SAFIR_SDK_VARIABLE" NOW PLEASE)
set(SAFIR_RUNTIME "STOP_USING_SAFIR_RUNTIME_VARIABLE" NOW PLEASE)

#set up install directories
SET(SAFIR_INSTALL_DESTINATION_INCLUDE include)
SET(SAFIR_INSTALL_DESTINATION_BIN bin)
SET(SAFIR_INSTALL_DESTINATION_LIB lib)

if (UNIX)
  SET(SAFIR_INSTALL_DESTINATION_CMAKE lib/cmake/SafirSDKCore)
  SET(SAFIR_INSTALL_DESTINATION_DOC share/doc/safir_sdk_core)
  SET(SAFIR_INSTALL_DESTINATION_MAN share/man)
  SET(SAFIR_INSTALL_DESTINATION_CSHARP lib/safir_sdk_core)
  SET(SAFIR_INSTALL_DESTINATION_JAR share/java/safir_sdk_core)
  SET(SAFIR_INSTALL_DESTINATION_TEST_DATA share/safir_sdk_core/test_data)
  SET(SAFIR_INSTALL_DESTINATION_GENERATION share/safir_sdk_core/generation)
  SET(SAFIR_INSTALL_DESTINATION_DOU_BASE share/safir_sdk_core/dou)
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


#TODO: should dobexplorer be in dev package?