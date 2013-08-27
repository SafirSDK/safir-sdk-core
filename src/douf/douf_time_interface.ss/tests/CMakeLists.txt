cmake_minimum_required(VERSION 2.8)

project(douf_test CXX)

#this project contains a library that has a C interface, so we don't want the "d" suffix on debug builds on windows
#Must be set before including cmake_settings.txt
SET(NO_LIBRARY_POSTFIXES 1) 

include($ENV{SAFIR_SDK}/data/build/cmake_settings.txt)

find_package(Boost COMPONENTS date_time)

find_package(PythonInterp)
FIND_SOURCE_ROOT(SOURCE_ROOT)

enable_testing()

add_subdirectory(AceTimeConverter_test)
add_subdirectory(ExternalTimeProvider_test)
add_subdirectory(TimeProvider_cpp_test)
add_subdirectory(TimeProvider_dotnet_test)

if (SAFIR_ADA_SUPPORT)
  add_subdirectory(TimeProvider_ada_test)
endif()

if (SAFIR_JAVA_SUPPORT)
  add_subdirectory(TimeProvider_java_test)
endif()
