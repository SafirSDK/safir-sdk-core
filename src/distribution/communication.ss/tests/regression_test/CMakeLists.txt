FILE(GLOB headers *.h)
FILE(GLOB sources *.cpp)

ADD_EXECUTABLE(regression_test ${sources} ${headers})

TARGET_LINK_LIBRARIES(regression_test PRIVATE
  communication
  lluf_internal
  Boost::thread)

SAFIR_INSTALL(TARGETS regression_test COMPONENT TestSuite)
