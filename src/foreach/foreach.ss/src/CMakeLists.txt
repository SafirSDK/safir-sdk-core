FILE(GLOB headers *.h)
FILE(GLOB sources *.cpp)

ADD_EXECUTABLE(foreach ${sources} ${headers})

TARGET_LINK_LIBRARIES(foreach PRIVATE
  swre_interface_cpp
  dose_cpp
  dots_generated-foreach-cpp
  logging_cpp
  ${Boost_SYSTEM_LIBRARY})

SAFIR_INSTALL(TARGETS foreach)
