#We need a very recent cmake if we're to use a visual studio project
#The reason is that test property generator expressions are only 
#supported in cmake 3.
if(MSVC AND CMAKE_GENERATOR MATCHES "Visual Studio")
  cmake_minimum_required(VERSION 3.0.0)
else()
  cmake_minimum_required(VERSION 2.8.12)
endif()

PROJECT(safir_sdk_core CXX C)


#TODO: !!
set(SAFIR_SDK "STOP_USING_SAFIR_SDK_VARIABLE" NOW PLEASE)
set(SAFIR_RUNTIME "STOP_USING_SAFIR_RUNTIME_VARIABLE" NOW PLEASE)

#set up install directories
SET(SAFIR_INSTALL_DESTINATION_INCLUDE include)
SET(SAFIR_INSTALL_DESTINATION_BIN bin)
SET(SAFIR_INSTALL_DESTINATION_LIB lib)
SET(SAFIR_INSTALL_DESTINATION_CMAKE lib/cmake/SafirSDKCore)
SET(SAFIR_INSTALL_DESTINATION_DOC share/doc/safir_sdk_core)
SET(SAFIR_INSTALL_DESTINATION_CSHARP_DLL lib/safir_sdk_core)
SET(SAFIR_INSTALL_DESTINATION_JAR share/java/safir_sdk_core)
SET(SAFIR_INSTALL_DESTINATION_GENERATION share/safir_sdk_core/generation)
SET(SAFIR_INSTALL_DESTINATION_DOU_BASE share/safir_sdk_core)

SET(CMAKE_MODULE_PATH ${CMAKE_CURRENT_SOURCE_DIR}/src/cmake ${CMAKE_MODULE_PATH})

INCLUDE_DIRECTORIES(include)
INSTALL(DIRECTORY include/ DESTINATION ${SAFIR_INSTALL_DESTINATION_INCLUDE}
  PATTERN ".svn" EXCLUDE
  PATTERN "*~" EXCLUDE)

ENABLE_TESTING()

add_subdirectory(src)
add_subdirectory(docs)
