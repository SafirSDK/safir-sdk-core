file(GLOB headers *.h)
file(GLOB sources *.cpp)

include_directories(${CMAKE_CURRENT_SOURCE_DIR}/../include)

add_executable(safir_websocket ${sources} ${headers})

TARGET_LINK_LIBRARIES(safir_websocket PRIVATE
  safir_generated-Core-cpp
  dots_internal
  lluf_utils
  dose_cpp)

SAFIR_INSTALL(TARGETS safir_websocket)
