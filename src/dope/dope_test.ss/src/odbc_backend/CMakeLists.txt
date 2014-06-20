ADD_EXECUTABLE(run_sql run_sql.cpp)

TARGET_LINK_LIBRARIES(run_sql PRIVATE
  dots_cpp
  olib
  ${SAFIR_ODBC_LIBRARIES}
  ${Boost_PROGRAM_OPTIONS_LIBRARY}
)

#TODO: install proper bits of this and provide a test script.


# find_package(PythonInterp)

# enable_testing()

# if("$ENV{Driver}" STREQUAL "")
#   set(ENV{Driver} mysql)
#   message("'Driver' environment variable is not set, using '$ENV{Driver}'")
# endif()

# if("$ENV{DATABASE_NAME}" STREQUAL "")
#   set(ENV{DATABASE_NAME} SafirDb)
#   message("'DATABASE_NAME' environment variable is not set, using '$ENV{DATABASE_NAME}'")
# endif()

# if("$ENV{DATABASE_SERVER}" STREQUAL "")
#   set(ENV{DATABASE_SERVER} localhost)
#   message("'DATABASE_SERVER' environment variable is not set, using '$ENV{DATABASE_SERVER}'")
# endif()

# ADD_TEST(NAME dope_odbc_backend_test COMMAND ${PYTHON_EXECUTABLE} ${CMAKE_CURRENT_SOURCE_DIR}/run_odbc_backend_test.py --driver $ENV{Driver} --database $ENV{DATABASE_NAME} --hostname $ENV{DATABASE_SERVER})

# #Long timeouts are needed on slow machines.
# SET_SAFIR_TEST_PROPERTIES(TEST dope_odbc_backend_test TIMEOUT 1800)
