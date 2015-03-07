ADD_EXECUTABLE(run_sql run_sql.cpp)

TARGET_LINK_LIBRARIES(run_sql PRIVATE
  olib
  ${Boost_PROGRAM_OPTIONS_LIBRARY}
)

SAFIR_INSTALL(TARGETS run_sql TEST_SUITE)

configure_file(run_dope_odbc_backend_test.py.in
  "${CMAKE_CURRENT_BINARY_DIR}/run_dope_odbc_backend_test.py" @ONLY)

SAFIR_INSTALL_PYTHON(SCRIPT ${CMAKE_CURRENT_BINARY_DIR}/run_dope_odbc_backend_test.py
  COMPONENT Test)
