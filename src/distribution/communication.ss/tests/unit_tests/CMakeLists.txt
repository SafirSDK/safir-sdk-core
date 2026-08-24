add_definitions(-DSAFIR_TEST)

file(GLOB headers *.h)
file(GLOB sources *.cpp)

INCLUDE_DIRECTORIES(${CMAKE_CURRENT_BINARY_DIR} ../../src/include)

add_executable(communication_unit_tests ${sources} ${headers} ../../src/MessageQueue.h
                ../../src/DeliveryHandler.h ../../src/DataSender.h
                ../../src/Node.h ../../src/NodeType.h
                ../../src/Writer.h ../../src/HeartbeatSender.h
                ../../src/DataReceiver.h ../../src/Discoverer.h ../../src/Utilities.h
                ../../src/CommunicationMessage.proto)

PROTOBUF_GENERATE(TARGET communication_unit_tests APPEND_PATH OUT_VAR generated_files)

if (MSVC)
  SET_SOURCE_FILES_PROPERTIES(${generated_files} PROPERTIES COMPILE_FLAGS "/wd4127 /wd4244 /wd4267 /wd4100 /wd4702 /wd4141 /wd4189")
endif()


target_link_libraries(communication_unit_tests PRIVATE
  lluf_internal
  lluf_id
  protobuf::protobuf
  Boost::regex
  # Resolver.h calls GetAdaptersAddresses() on Windows.
  $<$<PLATFORM_ID:Windows>:iphlpapi>)

#These tests are slow and system-level, so they are not run as part of ctest.
#Instead the self-contained binary and a thin driver script that runs all of its
#test suites are installed into the TestSuite component and run against an
#installed package, just like the other migrated slow tests. The binary uses only
#stub send policies or unicast loopback sockets, so it needs no runtime config.
SAFIR_INSTALL(TARGETS communication_unit_tests COMPONENT TestSuite)

configure_file(run_communication_tests.py.in
  "${CMAKE_CURRENT_BINARY_DIR}/run_communication_tests.py" @ONLY)
SAFIR_INSTALL_PYTHON(SCRIPT ${CMAKE_CURRENT_BINARY_DIR}/run_communication_tests.py
  COMPONENT TestSuite)
