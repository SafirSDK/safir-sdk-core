FILE(GLOB sources *.cpp ../common/*.cpp)
FILE(GLOB headers *.h ../common/*.h)

ADD_EXECUTABLE(MessageLatencies ${sources} ${headers})

TARGET_LINK_LIBRARIES(MessageLatencies PRIVATE
  dose_cpp
  safir_generated-DoseTest-cpp
  lluf_utils
  ${Boost_PROGRAM_OPTIONS_LIBRARY})

SAFIR_INSTALL(TARGETS MessageLatencies COMPONENT TestSuite)
