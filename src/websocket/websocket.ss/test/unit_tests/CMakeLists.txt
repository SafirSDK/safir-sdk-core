file(GLOB headers *.h)
file(GLOB sources *.cpp)

add_definitions(-D_WEBSOCKETPP_NO_CPP11_CHRONO_)
include_directories(${CMAKE_CURRENT_SOURCE_DIR}/../../include)

add_executable(safir_websocket_unittests 
                ${sources} ${headers} 
                ../../src/JsonHelpers.h
                ../../src/JsonRpcRequest.h
                ../../src/JsonRpcResponse.h
                ../../src/JsonRpcNotification.h
                ../../src/PingHandler.h
                ../../src/CommandValidator.h)

target_link_libraries(safir_websocket_unittests PRIVATE
  safir_generated-Core-cpp
  dots_internal)

add_test(NAME WebsocketUnitTests COMMAND safir_websocket_unittests)
SET_SAFIR_TEST_PROPERTIES(TEST WebsocketUnitTests TIMEOUT 360)
