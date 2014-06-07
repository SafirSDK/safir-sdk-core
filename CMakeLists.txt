cmake_minimum_required(VERSION 2.8.12)

#TODO: require 3.0 on windows (for the generator expressions on test properties)

PROJECT(safir_sdk_core CXX C)


#TODO: !!
set(SAFIR_SDK "STOP_USING_SAFIR_SDK_VARIABLE" NOW PLEASE)
set(SAFIR_RUNTIME "STOP_USING_SAFIR_RUNTIME_VARIABLE" NOW PLEASE)

SET(CMAKE_MODULE_PATH ${CMAKE_CURRENT_SOURCE_DIR}/src/cmake ${CMAKE_MODULE_PATH})

INCLUDE_DIRECTORIES(include)
INSTALL(DIRECTORY include DESTINATION include
  PATTERN ".svn" EXCLUDE
  PATTERN "*~" EXCLUDE)

ENABLE_TESTING()

add_subdirectory(src)
add_subdirectory(docs)
