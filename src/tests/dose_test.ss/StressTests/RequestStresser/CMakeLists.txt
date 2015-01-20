FILE(GLOB sources *.cpp ../common/*.cpp)
FILE(GLOB headers *.h ../common/*.h)

ADD_EXECUTABLE(RequestStresser ${sources} ${headers})

TARGET_LINK_LIBRARIES(RequestStresser PRIVATE
  dose_cpp
  safir_generated-DoseTest-cpp
  lluf_utils
  ${Boost_PROGRAM_OPTIONS_LIBRARY})


SAFIR_INSTALL(TARGETS RequestStresser TEST_SUITE)
