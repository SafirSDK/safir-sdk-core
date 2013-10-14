FILE(GLOB sources *.cpp)

ADD_EXECUTABLE(lll_test ${sources})

IF (NOT MSVC)
  TARGET_LINK_LIBRARIES(lll_test lluf_utils ${Boost_LIBRARIES})
ENDIF()

ADD_TEST(LowLevelLogger ${PYTHON_EXECUTABLE} ${CMAKE_CURRENT_SOURCE_DIR}/run_test.py ${CMAKE_CURRENT_SOURCE_DIR}/configs)

