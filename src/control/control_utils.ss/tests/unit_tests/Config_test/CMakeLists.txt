FILE(GLOB sources *.cpp)

ADD_EXECUTABLE(Config_test ${sources})

IF (NOT MSVC)
  TARGET_LINK_LIBRARIES(Config_test dots_internal lluf_id lluf_utils lluf_config ${Boost_LIBRARIES})
ENDIF ()

ADD_TEST(Config ${PYTHON_EXECUTABLE} ${CMAKE_CURRENT_SOURCE_DIR}/run_test.py ${CMAKE_CURRENT_SOURCE_DIR}/testconf)
