add_definitions(-DSAFIR_TEST)

ADD_EXECUTABLE(Distribution_test Distribution_test.cpp ../../src/Distribution.h)
ADD_EXECUTABLE(PoolHandler_test PoolHandlerTest.cpp
                ../../src/PoolDistributionRequestSender.h
                ../../src/PoolDistributionHandler.h
                ../../src/PoolSyncInfo.proto)

PROTOBUF_GENERATE(TARGET PoolHandler_test APPEND_PATH OUT_VAR generated_files)

if (MSVC)
  SET_SOURCE_FILES_PROPERTIES(${generated_files} PROPERTIES COMPILE_FLAGS "/wd4127 /wd4244 /wd4267 /wd4100 /wd4702")
endif()

TARGET_LINK_LIBRARIES(Distribution_test PRIVATE
  lluf_internal
  communication
  system_picture
  control_utils
  Boost::unit_test_framework)

TARGET_LINK_LIBRARIES(PoolHandler_test PRIVATE
  communication  
  lluf_internal
  dose_internal
  protobuf::protobuf
  Boost::thread
  Boost::unit_test_framework)


ADD_TEST(NAME Distribution_test COMMAND Distribution_test)
SET_SAFIR_TEST_PROPERTIES(TEST Distribution_test TIMEOUT 600)

ADD_TEST(NAME PoolHandler_test COMMAND PoolHandler_test)
SET_SAFIR_TEST_PROPERTIES(TEST PoolHandler_test TIMEOUT 600)

