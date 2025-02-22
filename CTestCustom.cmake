cmake_policy(SET CMP0012 NEW)
if ((DEFINED ENV{SAFIR_SKIP_SLOW_TESTS}) AND ($ENV{SAFIR_SKIP_SLOW_TESTS}))
  MESSAGE(WARNING "ENVIRONMENT VARIABLE SAFIR_SKIP_SLOW_TESTS IS SET! SKIPPING SOME TESTS!")
  set(CTEST_CUSTOM_TESTS_IGNORE
    LowLevelLogger
    Communication_DiscovererTest
    Communication_DataSenderTest
    ElectionHandler_test
    ElectionHandler_test_with_overflows
    RawHandler_test
    LamportClocks
    dope_file_backend_test
    dope_none_backend_test
    restart_nodes_with_entity_updates_only
    restart_nodes_with_entity_requests_only
    restart_nodes_with_entity_requests_and_updates
    light_nodes_clear_state
    light_nodes_keep_state
    StopHandler_test
    tracer_backdoor
    websocket_component_test
    websocket_stress_test
    Incarnation_And_Control_Tests
    system_picture_component_test
    light_nodes_smart_sync
    low_memory
    lowmem_basic_operations
    )
endif()
