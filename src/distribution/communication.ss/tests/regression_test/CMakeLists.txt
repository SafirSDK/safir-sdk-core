FILE(GLOB headers *.h)
FILE(GLOB sources *.cpp)

ADD_EXECUTABLE(communication_regression_test ${sources} ${headers})

TARGET_LINK_LIBRARIES(communication_regression_test PRIVATE
  communication
  lluf_internal
  Boost::thread)

SAFIR_INSTALL(TARGETS communication_regression_test COMPONENT TestSuite)
