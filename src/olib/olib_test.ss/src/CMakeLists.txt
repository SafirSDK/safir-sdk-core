cmake_minimum_required(VERSION 2.8)

project(OlibTester CXX)


INCLUDE_DIRECTORIES(.)

INCLUDE($ENV{SAFIR_SDK}/data/build/safir_sdk_core_config.cmake)

FILE(GLOB_RECURSE headers *.h)
FILE(GLOB sources *.cpp)

#FIND_PACKAGE(Boost COMPONENTS filesystem program_options system)

add_executable(OlibTester ${sources} ${headers})

if (NOT MSVC)
  TARGET_LINK_LIBRARIES(OlibTester PRIVATE olib swre_interface_cpp dots_generated-Safir-cpp dots_cpp ${SAFIR_ODBC_LIBRARIES} ${Boost_LIBRARIES})
endif()

enable_testing()

if("$ENV{Driver}" STREQUAL "")
  set(ENV{Driver} mimer)
  message("'Driver' environment variable is not set, using '$ENV{Driver}'")
endif()

if(NOT "$ENV{Driver}" STREQUAL "mimer")
  message("Olib currently only has complete support for mimer database")
  return()
endif()


if("$ENV{DATABASE_NAME}" STREQUAL "")
  set(ENV{DATABASE_NAME} SafirDb)
  message("'DATABASE_NAME' environment variable is not set, using '$ENV{DATABASE_NAME}'")
endif()

if("$ENV{DATABASE_SERVER}" STREQUAL "")
  set(ENV{DATABASE_SERVER} localhost)
  message("'DATABASE_SERVER' environment variable is not set, using '$ENV{DATABASE_SERVER}'")
endif()

ADD_TEST(NAME OlibTest_Connect COMMAND  OlibTester --testcase connect --driver $ENV{Driver} --hostname $ENV{DATABASE_SERVER} --database $ENV{DATABASE_NAME})
ADD_TEST(NAME OlibTest_Disconnect COMMAND  OlibTester --testcase disconnect --driver $ENV{Driver} --hostname $ENV{DATABASE_SERVER} --database $ENV{DATABASE_NAME})
ADD_TEST(NAME OlibTest_AllocCloseStatement COMMAND  OlibTester --testcase allocclosestm --driver $ENV{Driver} --hostname $ENV{DATABASE_SERVER} --database $ENV{DATABASE_NAME})
ADD_TEST(NAME OlibTest_ConnectionPooling COMMAND  OlibTester --testcase connectionpooling --driver $ENV{Driver} --hostname $ENV{DATABASE_SERVER} --database $ENV{DATABASE_NAME})
ADD_TEST(NAME OlibTest_ConnectionTimeout COMMAND  OlibTester --testcase connectiontimeout --driver $ENV{Driver} --hostname $ENV{DATABASE_SERVER} --database $ENV{DATABASE_NAME})
ADD_TEST(NAME OlibTest_QueryTimeout COMMAND  OlibTester --testcase readalltimeout --driver $ENV{Driver} --hostname $ENV{DATABASE_SERVER} --database $ENV{DATABASE_NAME})
ADD_TEST(NAME OlibTest_ClearTable COMMAND  OlibTester --testcase cleartable --driver $ENV{Driver} --hostname $ENV{DATABASE_SERVER} --database $ENV{DATABASE_NAME})
ADD_TEST(NAME OlibTest_CreateData COMMAND  OlibTester --testcase createdata --driver $ENV{Driver} --hostname $ENV{DATABASE_SERVER} --database $ENV{DATABASE_NAME})
ADD_TEST(NAME OlibTest_ReadData COMMAND  OlibTester --testcase readdata --driver $ENV{Driver} --hostname $ENV{DATABASE_SERVER} --database $ENV{DATABASE_NAME})
ADD_TEST(NAME OlibTest_UpdateData COMMAND  OlibTester --testcase updatedata --driver $ENV{Driver} --hostname $ENV{DATABASE_SERVER} --database $ENV{DATABASE_NAME})
ADD_TEST(NAME OlibTest_InsertInto42 COMMAND  OlibTester --testcase insertinto42 --driver $ENV{Driver} --hostname $ENV{DATABASE_SERVER} --database $ENV{DATABASE_NAME})
ADD_TEST(NAME OlibTest_DeleteData COMMAND  OlibTester --testcase deletedata --driver $ENV{Driver} --hostname $ENV{DATABASE_SERVER} --database $ENV{DATABASE_NAME})
ADD_TEST(NAME OlibTest_BinaryWriteRead COMMAND  OlibTester --testcase binaryrw --driver $ENV{Driver} --hostname $ENV{DATABASE_SERVER} --database $ENV{DATABASE_NAME})
ADD_TEST(NAME OlibTest_BlobWriteRead COMMAND  OlibTester --testcase blobrw --driver $ENV{Driver} --hostname $ENV{DATABASE_SERVER} --database $ENV{DATABASE_NAME})
ADD_TEST(NAME OlibTest_NClobWriteRead COMMAND  OlibTester --testcase nclobrw --driver $ENV{Driver} --hostname $ENV{DATABASE_SERVER} --database $ENV{DATABASE_NAME})
ADD_TEST(NAME OlibTest_LotsOfInput COMMAND  OlibTester --testcase lotsofinput --driver $ENV{Driver} --hostname $ENV{DATABASE_SERVER} --database $ENV{DATABASE_NAME})
ADD_TEST(NAME OlibTest_OutParam COMMAND  OlibTester --testcase outparam --driver $ENV{Driver} --hostname $ENV{DATABASE_SERVER} --database $ENV{DATABASE_NAME})
ADD_TEST(NAME OlibTest_InOutParam COMMAND  OlibTester --testcase inoutparam --driver $ENV{Driver} --hostname $ENV{DATABASE_SERVER} --database $ENV{DATABASE_NAME})
#ADD_TEST(NAME OlibTest_PerformanceTest COMMAND  OlibTester --testcase perftest --driver $ENV{Driver} --hostname $ENV{DATABASE_SERVER} --database $ENV{DATABASE_NAME})

SET_SAFIR_TEST_PROPERTIES(TEST OlibTest_Connect)
SET_SAFIR_TEST_PROPERTIES(TEST OlibTest_Disconnect)
SET_SAFIR_TEST_PROPERTIES(TEST OlibTest_AllocCloseStatement)
SET_SAFIR_TEST_PROPERTIES(TEST OlibTest_ConnectionPooling)
SET_SAFIR_TEST_PROPERTIES(TEST OlibTest_ConnectionTimeout)
SET_SAFIR_TEST_PROPERTIES(TEST OlibTest_QueryTimeout)
SET_SAFIR_TEST_PROPERTIES(TEST OlibTest_ClearTable)
SET_SAFIR_TEST_PROPERTIES(TEST OlibTest_CreateData)
SET_SAFIR_TEST_PROPERTIES(TEST OlibTest_ReadData)
SET_SAFIR_TEST_PROPERTIES(TEST OlibTest_UpdateData)
SET_SAFIR_TEST_PROPERTIES(TEST OlibTest_InsertInto42)
SET_SAFIR_TEST_PROPERTIES(TEST OlibTest_DeleteData)
SET_SAFIR_TEST_PROPERTIES(TEST OlibTest_BinaryWriteRead)
SET_SAFIR_TEST_PROPERTIES(TEST OlibTest_BlobWriteRead)
SET_SAFIR_TEST_PROPERTIES(TEST OlibTest_NClobWriteRead)
SET_SAFIR_TEST_PROPERTIES(TEST OlibTest_LotsOfInput)
SET_SAFIR_TEST_PROPERTIES(TEST OlibTest_OutParam)
SET_SAFIR_TEST_PROPERTIES(TEST OlibTest_InOutParam)
