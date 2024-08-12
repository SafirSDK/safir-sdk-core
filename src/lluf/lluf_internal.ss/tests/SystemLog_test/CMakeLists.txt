ADD_EXECUTABLE(SystemLog_test SystemLog_test.cpp)

TARGET_LINK_LIBRARIES(SystemLog_test PRIVATE lluf_internal)

ADD_TEST(NAME SystemLog_0
  COMMAND ${Python3_EXECUTABLE} ${CMAKE_CURRENT_SOURCE_DIR}/run_test.py ${CMAKE_CURRENT_SOURCE_DIR}/testconf/syslog_logging_0
  --test-exe $<TARGET_FILE:SystemLog_test>
  --test-exe-name $<TARGET_NAME:SystemLog_test>
)

ADD_TEST(NAME SystemLog_1
  COMMAND ${Python3_EXECUTABLE} ${CMAKE_CURRENT_SOURCE_DIR}/run_test.py ${CMAKE_CURRENT_SOURCE_DIR}/testconf/syslog_logging_1
  --test-exe $<TARGET_FILE:SystemLog_test>
  --test-exe-name $<TARGET_NAME:SystemLog_test>
)
ADD_TEST(NAME SystemLog_2
  COMMAND ${Python3_EXECUTABLE} ${CMAKE_CURRENT_SOURCE_DIR}/run_test.py ${CMAKE_CURRENT_SOURCE_DIR}/testconf/syslog_logging_2
  --test-exe $<TARGET_FILE:SystemLog_test>
  --test-exe-name $<TARGET_NAME:SystemLog_test>
)

SET_SAFIR_TEST_PROPERTIES(TEST SystemLog_0)
SET_SAFIR_TEST_PROPERTIES(TEST SystemLog_1 ENVIRONMENT "SAFIR_INSTANCE=5")
SET_SAFIR_TEST_PROPERTIES(TEST SystemLog_2)
