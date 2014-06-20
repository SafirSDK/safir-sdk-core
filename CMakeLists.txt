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

SET(CMAKE_MODULE_PATH ${CMAKE_CURRENT_SOURCE_DIR}/src/cmake ${CMAKE_MODULE_PATH})

INCLUDE_DIRECTORIES(include)
INSTALL(DIRECTORY include DESTINATION include
  PATTERN ".svn" EXCLUDE
  PATTERN "*~" EXCLUDE)

ENABLE_TESTING()

add_subdirectory(src)
add_subdirectory(docs)
