cmake_minimum_required(VERSION 2.8)

project(dose_main_test CXX)

INCLUDE($ENV{SAFIR_SDK}/data/build/cmake_settings.txt)

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
set_tests_properties(TryStart_dose_main PROPERTIES TIMEOUT 120)

ADD_TEST(dose_main_timers timer_test)
set_tests_properties(dose_main_timers PROPERTIES TIMEOUT 120)
