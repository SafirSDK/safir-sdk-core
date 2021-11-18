file(GLOB headers *.h)
file(GLOB sources *.cpp)

add_executable(safir_websocket_testclient
                ${sources} ${headers})

target_link_libraries(safir_websocket_testclient  PRIVATE
    RapidJSON::RapidJSON
    websocketpp::websocketpp
    Boost::program_options
    Boost::thread
    Boost::random
    Boost::system)

ADD_TEST(NAME websocket_component_test
  COMMAND ${Python3_EXECUTABLE} ${CMAKE_CURRENT_SOURCE_DIR}/run_test.py
  --safir-websocket $<TARGET_FILE:safir_websocket>
  --websocket-test-client $<TARGET_FILE:safir_websocket_testclient>
  --safir-control $<TARGET_FILE:safir_control>
  --dose_main $<TARGET_FILE:dose_main>
  --dope_main $<TARGET_FILE:dope_main>
  --safir-show-config $<TARGET_FILE:safir_show_config>
  )

SET_SAFIR_TEST_PROPERTIES(TEST websocket_component_test
  TIMEOUT 600
  CONFIG_OVERRIDE ${CMAKE_CURRENT_SOURCE_DIR}/test_config)

