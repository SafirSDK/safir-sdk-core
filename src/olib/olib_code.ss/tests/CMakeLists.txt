cmake_minimum_required(VERSION 2.8)

project(OlibTester CXX)


INCLUDE_DIRECTORIES(.)

INCLUDE($ENV{SAFIR_SDK}/data/build/cmake_settings.txt)

if (UNIX)
    FIND_PACKAGE(IODBC)
    if (IODBC_FOUND)
      INCLUDE_DIRECTORIES(${IODBC_INCLUDE_DIRS})
      SET(ODBC_LIBRARIES ${IODBC_LIBRARIES})
      ADD_DEFINITIONS(-DODBC_IS_IODBC)
    else()
      message("iODBC not found, trying UnixODBC")
      FIND_PACKAGE(UnixODBC REQUIRED)
      INCLUDE_DIRECTORIES(${UnixODBC_INCLUDE_DIRS})
      SET(ODBC_LIBRARIES ${UnixODBC_LIBRARIES})
      ADD_DEFINITIONS(-DODBC_IS_UNIXODBC)
    endif()
endif()

FILE(GLOB_RECURSE headers *.h)
FILE(GLOB sources *.cpp)

FIND_PACKAGE(Boost COMPONENTS filesystem program_options system)

add_executable(OlibTester ${sources} ${headers})

if (NOT MSVC)
  TARGET_LINK_LIBRARIES(OlibTester olib swre_interface_cpp dots_generated-Safir-cpp dots_cpp ${ODBC_LIBRARIES} ${Boost_LIBRARIES})
endif()

enable_testing()

if("$ENV{Driver}" STREQUAL "")
  set(ENV{Driver} mysql)
  message("'Driver' environment variable is not set, using '$ENV{Driver}'")
endif()

if("$ENV{DATABASE_NAME}" STREQUAL "")
  set(ENV{DATABASE_NAME} SafirDb)
  message("'DATABASE_NAME' environment variable is not set, using '$ENV{DATABASE_NAME}'")
endif()

if("$ENV{DATABASE_SERVER}" STREQUAL "")
  set(ENV{DATABASE_SERVER} localhost)
  message("'DATABASE_SERVER' environment variable is not set, using '$ENV{DATABASE_SERVER}'")
endif()

ADD_TEST(OlibTest_Connect OlibTester --testcase connect --driver $ENV{Driver} --hostname $ENV{DATABASE_SERVER} --database $ENV{DATABASE_NAME})
ADD_TEST(OlibTest_Disconnect OlibTester --testcase disconnect --driver $ENV{Driver} --hostname $ENV{DATABASE_SERVER} --database $ENV{DATABASE_NAME})
ADD_TEST(OlibTest_AllocCloseStatement OlibTester --testcase allocclosestm --driver $ENV{Driver} --hostname $ENV{DATABASE_SERVER} --database $ENV{DATABASE_NAME})
ADD_TEST(OlibTest_ConnectionPooling   OlibTester --testcase connectionpooling --driver $ENV{Driver} --hostname $ENV{DATABASE_SERVER} --database $ENV{DATABASE_NAME})
ADD_TEST(OlibTest_ConnectionTimeout   OlibTester --testcase connectiontimeout --driver $ENV{Driver} --hostname $ENV{DATABASE_SERVER} --database $ENV{DATABASE_NAME})
ADD_TEST(OlibTest_QueryTimeout   OlibTester --testcase readalltimeout --driver $ENV{Driver} --hostname $ENV{DATABASE_SERVER} --database $ENV{DATABASE_NAME})
ADD_TEST(OlibTest_ClearTable OlibTester --testcase cleartable --driver $ENV{Driver} --hostname $ENV{DATABASE_SERVER} --database $ENV{DATABASE_NAME})
ADD_TEST(OlibTest_CreateData OlibTester --testcase createdata --driver $ENV{Driver} --hostname $ENV{DATABASE_SERVER} --database $ENV{DATABASE_NAME})
ADD_TEST(OlibTest_ReadData   OlibTester --testcase readdata --driver $ENV{Driver} --hostname $ENV{DATABASE_SERVER} --database $ENV{DATABASE_NAME})
ADD_TEST(OlibTest_UpdateData   OlibTester --testcase updatedata --driver $ENV{Driver} --hostname $ENV{DATABASE_SERVER} --database $ENV{DATABASE_NAME})
ADD_TEST(OlibTest_InsertInto42   OlibTester --testcase insertinto42 --driver $ENV{Driver} --hostname $ENV{DATABASE_SERVER} --database $ENV{DATABASE_NAME})
ADD_TEST(OlibTest_DeleteData   OlibTester --testcase deletedata --driver $ENV{Driver} --hostname $ENV{DATABASE_SERVER} --database $ENV{DATABASE_NAME})
ADD_TEST(OlibTest_BinaryWriteRead   OlibTester --testcase binaryrw --driver $ENV{Driver} --hostname $ENV{DATABASE_SERVER} --database $ENV{DATABASE_NAME})
ADD_TEST(OlibTest_BlobWriteRead   OlibTester --testcase blobrw --driver $ENV{Driver} --hostname $ENV{DATABASE_SERVER} --database $ENV{DATABASE_NAME})
ADD_TEST(OlibTest_NClobWriteRead   OlibTester --testcase nclobrw --driver $ENV{Driver} --hostname $ENV{DATABASE_SERVER} --database $ENV{DATABASE_NAME})
ADD_TEST(OlibTest_LotsOfInput   OlibTester --testcase lotsofinput --driver $ENV{Driver} --hostname $ENV{DATABASE_SERVER} --database $ENV{DATABASE_NAME})
ADD_TEST(OlibTest_OutParam   OlibTester --testcase outparam --driver $ENV{Driver} --hostname $ENV{DATABASE_SERVER} --database $ENV{DATABASE_NAME})
ADD_TEST(OlibTest_InOutParam   OlibTester --testcase inoutparam --driver $ENV{Driver} --hostname $ENV{DATABASE_SERVER} --database $ENV{DATABASE_NAME})
#ADD_TEST(OlibTest_PerformanceTest   OlibTester --testcase perftest --driver $ENV{Driver} --hostname $ENV{DATABASE_SERVER} --database $ENV{DATABASE_NAME})
