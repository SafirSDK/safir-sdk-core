FILE(GLOB sources *.cpp ../common/*.cpp)
FILE(GLOB headers *.h ../common/*.h)

ADD_EXECUTABLE(MessageStresser ${sources} ${headers})

TARGET_LINK_LIBRARIES(MessageStresser PRIVATE
  dose_cpp
  safir_generated-DoseTest-cpp
  lluf_utils
  ${Boost_PROGRAM_OPTIONS_LIBRARY})

SAFIR_INSTALL(TARGETS MessageStresser TEST_SUITE)
