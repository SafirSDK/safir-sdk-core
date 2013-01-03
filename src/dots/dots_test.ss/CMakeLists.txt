cmake_minimum_required(VERSION 2.8)

project(dots_test CXX)

INCLUDE($ENV{SAFIR_SDK}/data/build/cmake_settings.txt)

ADD_SUBDIRECTORY(cpp)
ADD_SUBDIRECTORY(dotnet)

if (SAFIR_ADA_SUPPORT)
  ADD_SUBDIRECTORY(ada)
endif()

if (SAFIR_JAVA_SUPPORT)
  ADD_SUBDIRECTORY(java)
endif()

FIND_PACKAGE(PythonInterp)

ENABLE_TESTING()
ADD_TEST(dots_cpp ${PYTHON_EXECUTABLE} ${CMAKE_CURRENT_SOURCE_DIR}/run_test.py cpp)

if (SAFIR_ADA_SUPPORT)
  ADD_TEST(dots_ada ${PYTHON_EXECUTABLE} ${CMAKE_CURRENT_SOURCE_DIR}/run_test.py ada)
endif()

ADD_TEST(dots_dotnet ${PYTHON_EXECUTABLE} ${CMAKE_CURRENT_SOURCE_DIR}/run_test.py dotnet)

if (SAFIR_JAVA_SUPPORT)
  ADD_TEST(dots_java ${PYTHON_EXECUTABLE} ${CMAKE_CURRENT_SOURCE_DIR}/run_test.py java)
endif()
