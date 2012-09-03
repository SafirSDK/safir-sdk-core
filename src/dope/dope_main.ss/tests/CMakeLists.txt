cmake_minimum_required(VERSION 2.8)

project(dope_test CXX)

include($ENV{SAFIR_SDK}/data/build/cmake_settings.txt)

find_package(Boost COMPONENTS system)

ADD_EXECUTABLE(entity_owner entity_owner.cpp)

if (NOT MSVC)
  TARGET_LINK_LIBRARIES(entity_owner dose_cpp dots_generated-DopeTest-cpp dots_cpp ${Boost_LIBRARIES})
endif()

find_package(PythonInterp)

enable_testing()

ADD_TEST(dope_test ${PYTHON_EXECUTABLE} ${CMAKE_CURRENT_SOURCE_DIR}/run_test.py)

set_tests_properties(dope_test PROPERTIES TIMEOUT 240)
