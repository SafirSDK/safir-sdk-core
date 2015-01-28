if ($ENV{SAFIR_SKIP_SLOW_TESTS})
  MESSAGE("Warning: ENVIRONMENT VARIABLE SAFIR_SKIP_SLOW_TESTS IS SET! SKIPPING SOME TESTS!")
  set(CTEST_CUSTOM_TESTS_IGNORE
    LowLevelLogger
    ElectionHandler_test
    ElectionHandler_test_with_overflows
    LamportClocks
    dose_main_timers
    dope_file_backend_test
    )
endif()
