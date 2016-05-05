file(GLOB headers *.h)
file(GLOB sources *.cpp)

add_executable(safir_websocket_unittests 
                ${sources} ${headers} 
                ../../src/JsonHelpers.h
                ../../src/CommandValidator.h)

target_link_libraries(safir_websocket_unittests PRIVATE
  safir_generated-Core-cpp
  dots_internal)

add_test(NAME WebsocketUnitTests COMMAND safir_websocket_unittests)
SET_SAFIR_TEST_PROPERTIES(TEST WebsocketUnitTests TIMEOUT 360)
