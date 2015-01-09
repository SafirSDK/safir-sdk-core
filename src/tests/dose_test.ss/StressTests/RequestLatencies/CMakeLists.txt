FILE(GLOB sources *.cpp ../common/*.cpp)
FILE(GLOB headers *.h ../common/*.h)

ADD_EXECUTABLE(RequestLatencies ${sources} ${headers})


TARGET_LINK_LIBRARIES(RequestLatencies PRIVATE
  dose_cpp
  safir_generated-DoseTest-cpp
  ${Boost_PROGRAM_OPTIONS_LIBRARY}
  ${Boost_THREAD_LIBRARY}
  ${Boost_SYSTEM_LIBRARY})


SAFIR_INSTALL(TARGETS RequestLatencies TEST_SUITE)
