FILE(GLOB headers *.h)
FILE(GLOB sources *.cpp)

ADD_EXECUTABLE(dose_test_sequencer ${sources} ${headers})

TARGET_LINK_LIBRARIES(dose_test_sequencer PRIVATE
  dose_cpp
  safir_generated-DoseTest-cpp
  Boost::regex
  Boost::filesystem
  Boost::program_options)

SAFIR_INSTALL(TARGETS dose_test_sequencer COMPONENT TestSuite)
