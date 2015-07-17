FILE(GLOB sources *.cpp ../common/*.cpp)
FILE(GLOB headers *.h ../common/*.h)

ADD_EXECUTABLE(LargeMessageSender ${sources} ${headers})

TARGET_LINK_LIBRARIES(LargeMessageSender PRIVATE
  dose_cpp
  safir_generated-DoseTest-cpp
  lluf_utils
  ${Boost_PROGRAM_OPTIONS_LIBRARY})

SAFIR_INSTALL(TARGETS LargeMessageSender TEST_SUITE)
