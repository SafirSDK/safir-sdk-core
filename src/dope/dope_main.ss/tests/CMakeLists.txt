cmake_minimum_required(VERSION 2.8)

project(dope_test CXX)

include($ENV{SAFIR_SDK}/data/build/cmake_settings.txt)

find_package(Boost COMPONENTS program_options system)

if (UNIX)
    FIND_PACKAGE(IODBC)
    if (IODBC_FOUND)
      INCLUDE_DIRECTORIES(${IODBC_INCLUDE_DIRS})
      SET(ODBC_LIBRARIES ${IODBC_LIBRARIES})
      message("iODBC not found, trying UnixODBC")
    else()
      FIND_PACKAGE(UnixODBC REQUIRED)
      INCLUDE_DIRECTORIES(${UnixODBC_INCLUDE_DIRS})
      SET(ODBC_LIBRARIES ${UnixODBC_LIBRARIES})
    endif()
endif()


ADD_EXECUTABLE(entity_owner entity_owner.cpp)
ADD_EXECUTABLE(run_sql run_sql.cpp)

if (NOT MSVC)
  TARGET_LINK_LIBRARIES(entity_owner dose_cpp dots_generated-DopeTest-cpp dots_cpp ${Boost_LIBRARIES})
  TARGET_LINK_LIBRARIES(run_sql dots_cpp olib ${Boost_LIBRARIES} ${ODBC_LIBRARIES})
endif()

find_package(PythonInterp)

enable_testing()

ADD_TEST(dope_file_backend_test ${PYTHON_EXECUTABLE} ${CMAKE_CURRENT_SOURCE_DIR}/run_file_backend_test.py)
#ADD_TEST(dope_odbc_backend_test ${PYTHON_EXECUTABLE} ${CMAKE_CURRENT_SOURCE_DIR}/run_odbc_backend_test.py)

#Long timeouts are needed on slow machines.
set_tests_properties(dope_file_backend_test PROPERTIES TIMEOUT 1800)
#set_tests_properties(dope_odbc_backend_test PROPERTIES TIMEOUT 1800)
