FILE(GLOB sources *.cpp ../common/*.cpp)
FILE(GLOB headers *.h ../common/*.h)

ADD_EXECUTABLE(SignalTest ${sources} ${headers})

TARGET_LINK_LIBRARIES(SignalTest PRIVATE
  dose_internal
  ${Boost_PROGRAM_OPTIONS_LIBRARY}
  lluf_utils)

SAFIR_INSTALL(TARGETS SignalTest TEST_SUITE)
