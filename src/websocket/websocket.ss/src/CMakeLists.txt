file(GLOB headers *.h)
file(GLOB sources *.cpp)

add_definitions(-D_WEBSOCKETPP_NO_CPP11_CHRONO_)
include_directories(${CMAKE_CURRENT_SOURCE_DIR}/../include)

add_executable(safir_websocket ${sources} ${headers})

target_link_libraries(safir_websocket PRIVATE
  safir_generated-Core-cpp
  dots_internal
  lluf_utils
  dose_cpp)

SAFIR_INSTALL(TARGETS safir_websocket)
