FILE(GLOB sources *.cpp)

ADD_EXECUTABLE(Config_test ${sources})

TARGET_LINK_LIBRARIES(Config_test
  lluf_id
  lluf_config
  dots_internal
  )

ADD_TEST(Config ${PYTHON_EXECUTABLE} ${CMAKE_CURRENT_SOURCE_DIR}/run_test.py ${CMAKE_CURRENT_SOURCE_DIR}/testconf)
