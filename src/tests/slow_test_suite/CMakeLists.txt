#The slow, system-level "population 1" tests are not run as part of ctest;
#instead each is installed into the TestSuite component and run against an
#installed package, just like the dose tests (see src/tests/dose_test.ss). This
#umbrella driver invokes all of those per-test drivers in turn, so the whole
#slow suite can be run locally with a single command after installing the
#package. It is installed to bin as run_slow_tests, alongside run_dose_tests.
configure_file(run_slow_tests.py.in
  "${CMAKE_CURRENT_BINARY_DIR}/run_slow_tests.py" @ONLY)

SAFIR_INSTALL_PYTHON(SCRIPT ${CMAKE_CURRENT_BINARY_DIR}/run_slow_tests.py
  COMPONENT TestSuite)
