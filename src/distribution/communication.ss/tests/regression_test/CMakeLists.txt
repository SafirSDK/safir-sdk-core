FILE(GLOB headers *.h)
FILE(GLOB sources *.cpp)

ADD_EXECUTABLE(regression_test ${sources} ${headers})

TARGET_LINK_LIBRARIES(regression_test PRIVATE
  communication
  ${Boost_THREAD_LIBRARY}
  ${Boost_SYSTEM_LIBRARY})

SAFIR_INSTALL(TARGETS regression_test TEST_SUITE)
