file(GLOB headers *.h)
file(GLOB sources *.cpp)

add_executable(safir_websocket ${sources} ${headers})

target_link_libraries(safir_websocket PRIVATE
  swre_interface_cpp
  dose_cpp
  safir_generated-Core-cpp
  dots_internal
  lluf_internal
  rapidjson
  websocketpp::websocketpp)

SAFIR_INSTALL(TARGETS safir_websocket)
