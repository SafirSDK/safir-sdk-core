FILE(GLOB sources *.cpp)

ADD_EXECUTABLE(ss_test ${sources})

IF (NOT MSVC)
  TARGET_LINK_LIBRARIES(ss_test lluf_utils ${Boost_LIBRARIES})
ENDIF()

ADD_TEST(StartupSynchronizer ${PYTHON_EXECUTABLE} ${CMAKE_CURRENT_SOURCE_DIR}/run_test.py)
