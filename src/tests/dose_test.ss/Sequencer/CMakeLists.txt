FILE(GLOB headers *.h)
FILE(GLOB sources *.cpp)

ADD_EXECUTABLE(dose_test_sequencer ${sources} ${headers})

TARGET_LINK_LIBRARIES(dose_test_sequencer PRIVATE
  dose_cpp
  safir_generated-DoseTest-cpp
  ${Boost_REGEX_LIBRARY}
  ${Boost_FILESYSTEM_LIBRARY}
  ${Boost_PROGRAM_OPTIONS_LIBRARY})

SAFIR_INSTALL(TARGETS dose_test_sequencer COMPONENT TestSuite)
