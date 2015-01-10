FILE(GLOB headers *.h)
FILE(GLOB sources *.cpp)

ADD_EXECUTABLE(communication_test ${sources} ${headers})

TARGET_LINK_LIBRARIES(communication_test
  communication
  ${Boost_PROGRAM_OPTIONS_LIBRARY})

SAFIR_INSTALL(TARGETS communication_test TEST_SUITE)
