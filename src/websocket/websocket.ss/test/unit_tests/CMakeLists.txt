file(GLOB headers *.h)
file(GLOB sources *.cpp)

add_executable(safir_websocket_unittests
                ${sources} ${headers}
                ../../src/JsonHelpers.h
                ../../src/JsonRpcRequest.h
                ../../src/JsonRpcResponse.h
                ../../src/JsonRpcNotification.h
                ../../src/PingHandler.h
                ../../src/RequestIdMapper.h
                ../../src/ResponseSenderStore.h
                ../../src/IpAddressHelper.h
                ../../src/CommandValidator.h)

target_link_libraries(safir_websocket_unittests PRIVATE
    dose_cpp
    safir_generated-Core-cpp
    dots_internal
    RapidJSON::RapidJSON
    websocketpp::websocketpp)


set_property(TARGET safir_websocket_unittests PROPERTY CXX_STANDARD 17)

add_test(NAME WebsocketUnitTests COMMAND safir_websocket_unittests)
SET_SAFIR_TEST_PROPERTIES(TEST WebsocketUnitTests TIMEOUT 360)
