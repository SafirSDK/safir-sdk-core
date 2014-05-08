FILE(GLOB sources *.cpp)

ADD_EXECUTABLE(ProcessInfo_test ProcessInfo_test.cpp)
ADD_EXECUTABLE(ProcessInfoSleeper ProcessInfoSleeper.cpp)

IF (NOT MSVC)
  TARGET_LINK_LIBRARIES (ProcessInfoSleeper ${Boost_LIBRARIES})
  TARGET_LINK_LIBRARIES (ProcessInfo_test lluf_utils ${Boost_LIBRARIES})
ENDIF()

ADD_TEST(ProcessInfo ${PYTHON_EXECUTABLE} ${CMAKE_CURRENT_SOURCE_DIR}/run_test.py)
SET_SAFIR_TEST_PROPERTIES(ProcessInfo)
