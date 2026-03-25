file(GLOB headers *.h)
file(GLOB sources *.cpp)

list(APPEND headers DobConnectionRegistry.h)
list(APPEND sources DobConnectionRegistry.cpp)
list(APPEND headers RestServer.h)
list(APPEND sources RestServer.cpp)

add_executable(safir_web ${sources} ${headers})

target_link_libraries(safir_web PRIVATE
  swre_interface_cpp
  dose_cpp
  safir_generated-Core-cpp
  dots_internal
  lluf_internal
  rapidjson
  Boost::boost)

SAFIR_INSTALL(TARGETS safir_web)
