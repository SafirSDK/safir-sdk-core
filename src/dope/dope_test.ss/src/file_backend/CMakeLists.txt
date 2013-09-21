cmake_minimum_required(VERSION 2.8)

project(dope_file_backend_test CXX)

include($ENV{SAFIR_SDK}/data/build/cmake_settings.txt)

find_package(Boost COMPONENTS system)

ADD_EXECUTABLE(entity_owner ../entity_owner.cpp)

if (NOT MSVC)
  TARGET_LINK_LIBRARIES(entity_owner dose_cpp dots_generated-DopeTest-cpp dots_cpp ${Boost_LIBRARIES})
endif()

find_package(PythonInterp)

enable_testing()

ADD_TEST(dope_file_backend_test ${PYTHON_EXECUTABLE} ${CMAKE_CURRENT_SOURCE_DIR}/run_file_backend_test.py)

#Long timeouts are needed on slow machines.
SET_SAFIR_TEST_PROPERTIES(dope_file_backend_test 1800)
