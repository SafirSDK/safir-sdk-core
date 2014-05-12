FILE(GLOB headers *.h)
FILE(GLOB sources *.cpp)

ADD_EXECUTABLE(dose_test_sequencer ${sources} ${headers})

TARGET_LINK_LIBRARIES(dose_test_sequencer PRIVATE
  dose_cpp
  dose_internal
  dots_generated-DoseTest-cpp
  dots_generated-Safir-cpp
  dots_cpp 
  logging_cpp
  ${Boost_REGEX_LIBRARY}
  ${Boost_THREAD_LIBRARY}
  ${Boost_SYSTEM_LIBRARY}
  ${Boost_FILESYSTEM_LIBRARY}
  ${Boost_PROGRAM_OPTIONS_LIBRARY}
  ${Boost_TIMER_LIBRARY})

SAFIR_INSTALL(TARGETS dose_test_sequencer)
