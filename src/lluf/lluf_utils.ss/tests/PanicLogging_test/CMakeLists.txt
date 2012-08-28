FILE(GLOB sources *.cpp)

ADD_EXECUTABLE(pl_test ${sources})

IF (NOT MSVC)
  TARGET_LINK_LIBRARIES(pl_test lluf_utils ${Boost_LIBRARIES})
ENDIF ()

ADD_TEST(PanicLogging ${PYTHON_EXECUTABLE} ${CMAKE_CURRENT_SOURCE_DIR}/run_test.py)
