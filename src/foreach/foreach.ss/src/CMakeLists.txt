FILE(GLOB headers *.h)
FILE(GLOB sources *.cpp)

ADD_EXECUTABLE(foreach ${sources} ${headers})

TARGET_LINK_LIBRARIES(foreach PRIVATE
  swre_interface_cpp
  dose_cpp
  logging_cpp
  ${Boost_ATOMIC_LIBRARY})

SAFIR_INSTALL(TARGETS foreach)
