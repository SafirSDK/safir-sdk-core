cmake_minimum_required(VERSION 2.8)

project(stop_orders_at_exit CXX)

include($ENV{SAFIR_SDK}/data/build/safir_sdk_core_config.cmake)

#find_package(Boost COMPONENTS system)
find_package(PythonInterp)

add_executable(stoppee stoppee.cpp)

if (NOT MSVC)
  target_link_libraries(stoppee PRIVATE dose_cpp dots_generated-Safir-cpp dots_cpp ${Boost_LIBRARIES})
endif()

enable_testing()

ADD_TEST(stop_orders_at_exit ${PYTHON_EXECUTABLE} ${CMAKE_CURRENT_SOURCE_DIR}/run_test.py)

SET_SAFIR_TEST_PROPERTIES(TEST stop_orders_at_exit TIMEOUT 240)
