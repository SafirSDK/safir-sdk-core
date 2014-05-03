FILE(GLOB headers *.h)
FILE(GLOB sources *.cpp)

ADD_EXECUTABLE(foreach ${sources} ${headers})

TARGET_LINK_LIBRARIES(foreach PRIVATE
  swre_interface_cpp
  douf_time_cpp
  dose_internal
  dose_cpp
  dots_generated-foreach-cpp
  dots_generated-Safir-cpp
  dots_cpp
  logging_cpp
  ${Boost_SYSTEM_LIBRARY})

SAFIR_INSTALL(TARGETS foreach)
