file(GLOB headers *.h)
file(GLOB sources *.cpp)

add_executable(safir_websocket_testclient
                ${sources} ${headers})

target_link_libraries(safir_websocket_testclient  PRIVATE
    RapidJSON::RapidJSON
    websocketpp::websocketpp
    ${Boost_PROGRAM_OPTIONS_LIBRARY}
    ${Boost_RANDOM_LIBRARY}
    ${Boost_THREAD_LIBRARY}
    ${Boost_SYSTEM_LIBRARY})

get_property(SAFIR_GENERATED_PATHS GLOBAL PROPERTY SAFIR_GENERATED_PATHS)

ADD_TEST(NAME websocket_component_test
  COMMAND ${PYTHON_EXECUTABLE} ${CMAKE_CURRENT_SOURCE_DIR}/run_test.py
  --safir-websocket $<TARGET_FILE:safir_websocket>
  --websocket-test-client $<TARGET_FILE:safir_websocket_testclient>
  --safir-control $<TARGET_FILE:safir_control>
  --dose_main $<TARGET_FILE:dose_main>
  --dope_main $<TARGET_FILE:dope_main>
  --safir-show-config $<TARGET_FILE:safir_show_config>
  --safir-generated-paths "${SAFIR_GENERATED_PATHS}")

SET_SAFIR_TEST_PROPERTIES(TEST websocket_component_test
  TIMEOUT 600
  CONFIG_OVERRIDE ${CMAKE_CURRENT_SOURCE_DIR}/test_config)

