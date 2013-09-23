cmake_minimum_required(VERSION 2.8)

project(dose_main_test CXX)

INCLUDE($ENV{SAFIR_SDK}/data/build/safir_sdk_core_config.cmake)

ADD_EXECUTABLE(timer_test timer_test.cpp ../src/dose_main_timers.h ../src/dose_main_timers.cpp)

FIND_PACKAGE(Boost COMPONENTS thread system)

if (NOT MSVC)
  TARGET_LINK_LIBRARIES(timer_test 
    dots_cpp
    ${Boost_LIBRARIES})
endif()

ENABLE_TESTING()

FIND_PACKAGE(PythonInterp)

ADD_TEST(TryStart_dose_main  ${PYTHON_EXECUTABLE} ${CMAKE_CURRENT_SOURCE_DIR}/try_start_dose_main.py)
ADD_TEST(dose_main_timers timer_test)

SET_SAFIR_TEST_PROPERTIES(TryStart_dose_main 120)
SET_SAFIR_TEST_PROPERTIES(dose_main_timers 120)
