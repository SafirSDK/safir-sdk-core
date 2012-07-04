cmake_minimum_required(VERSION 2.6)

project(dots_test CXX Java)

INCLUDE($ENV{SAFIR_SDK}/data/build/cmake_settings.txt)

ADD_SUBDIRECTORY(cpp)
ADD_SUBDIRECTORY(dotnet)
ADD_SUBDIRECTORY(ada)
ADD_SUBDIRECTORY(java)

FIND_PACKAGE(PythonInterp)

ENABLE_TESTING()
ADD_TEST(dots_cpp ${PYTHON_EXECUTABLE} ${CMAKE_CURRENT_SOURCE_DIR}/run_test.py cpp)
ADD_TEST(dots_ada ${PYTHON_EXECUTABLE} ${CMAKE_CURRENT_SOURCE_DIR}/run_test.py ada)
ADD_TEST(dots_dotnet ${PYTHON_EXECUTABLE} ${CMAKE_CURRENT_SOURCE_DIR}/run_test.py dotnet)
ADD_TEST(dots_java ${PYTHON_EXECUTABLE} ${CMAKE_CURRENT_SOURCE_DIR}/run_test.py java)
