PROTOBUF_GENERATE_CPP(PROTO_SRCS PROTO_HDRS
  ../../src/RawStatisticsMessage.proto
  ../../src/SystemStateMessage.proto
  ../../src/ElectionMessage.proto)

if (MSVC)
  SET_SOURCE_FILES_PROPERTIES(${PROTO_SRCS} PROPERTIES COMPILE_FLAGS "/wd4127 /wd4244 /wd4267 /wd4018")
endif()

#Most of the tests in here need the generated protobuf headers
#and all need the SystemPicture headers
INCLUDE_DIRECTORIES(${CMAKE_CURRENT_BINARY_DIR} ../../src/include)

#Need to define this to not get inconsistent dll linkage on windows.
#no test is meant to actually link the system_picture library, so we can do this.
add_definitions(-Dsystem_picture_EXPORTS)

ADD_EXECUTABLE(RawStatistics_test RawStatistics_test.cpp ../../src/RawStatistics.cpp ${PROTO_SRCS} ${PROTO_HDRS})
ADD_EXECUTABLE(SystemState_test SystemState_test.cpp ../../src/SystemState.cpp ${PROTO_SRCS} ${PROTO_HDRS})
ADD_EXECUTABLE(RawPublisherLocal_test RawPublisherLocal_test.cpp ${PROTO_HDRS})
ADD_EXECUTABLE(RawPublisherRemote_test RawPublisherRemote_test.cpp ../../src/RawStatistics.cpp ${PROTO_SRCS}  ${PROTO_HDRS})
ADD_EXECUTABLE(StatePublisherLocal_test StatePublisherLocal_test.cpp ${PROTO_HDRS})
ADD_EXECUTABLE(StatePublisherRemote_test StatePublisherRemote_test.cpp ${PROTO_HDRS})

ADD_EXECUTABLE(LocalSubscriber_test LocalSubscriber_test.cpp ../../src/RawStatistics.cpp ${PROTO_SRCS} ${PROTO_HDRS})
ADD_EXECUTABLE(RemoteSubscriber_test RemoteSubscriber_test.cpp)

ADD_EXECUTABLE(RawHandler_test RawHandler_test.cpp ../../src/RawStatistics.cpp ${PROTO_SRCS} ${PROTO_HDRS})
ADD_EXECUTABLE(RawChanges_test RawChanges_test.cpp)
ADD_EXECUTABLE(ElectionHandler_test ElectionHandler_test.cpp ../../src/RawStatistics.cpp ${PROTO_SRCS} ${PROTO_HDRS})
ADD_EXECUTABLE(Coordinator_test Coordinator_test.cpp ../../src/RawStatistics.cpp ../../src/SystemState.cpp ${PROTO_SRCS} ${PROTO_HDRS})


TARGET_LINK_LIBRARIES(RawStatistics_test PRIVATE
  lluf_utils
  ${Boost_UNIT_TEST_FRAMEWORK_LIBRARY}
  ${PROTOBUF_LITE_LIBRARIES})

TARGET_LINK_LIBRARIES(SystemState_test PRIVATE
  lluf_utils
  ${Boost_UNIT_TEST_FRAMEWORK_LIBRARY}
  ${PROTOBUF_LITE_LIBRARIES})

TARGET_LINK_LIBRARIES(RawPublisherLocal_test PRIVATE
  lluf_utils
  ${Boost_UNIT_TEST_FRAMEWORK_LIBRARY})

TARGET_LINK_LIBRARIES(RawPublisherRemote_test PRIVATE
  communication
  lluf_utils
  lluf_id
  ${Boost_UNIT_TEST_FRAMEWORK_LIBRARY}
  ${PROTOBUF_LITE_LIBRARIES})

TARGET_LINK_LIBRARIES(StatePublisherLocal_test PRIVATE
  lluf_utils
  ${Boost_UNIT_TEST_FRAMEWORK_LIBRARY})

TARGET_LINK_LIBRARIES(StatePublisherRemote_test PRIVATE
  communication
  lluf_utils
  lluf_id
  ${Boost_UNIT_TEST_FRAMEWORK_LIBRARY})

TARGET_LINK_LIBRARIES(LocalSubscriber_test PRIVATE
  lluf_utils
  ${Boost_UNIT_TEST_FRAMEWORK_LIBRARY}
  ${PROTOBUF_LITE_LIBRARIES})

TARGET_LINK_LIBRARIES(RemoteSubscriber_test PRIVATE
  lluf_id
  lluf_utils
  ${Boost_UNIT_TEST_FRAMEWORK_LIBRARY})

TARGET_LINK_LIBRARIES(RawHandler_test PRIVATE
  lluf_utils
  ${Boost_UNIT_TEST_FRAMEWORK_LIBRARY}
  ${PROTOBUF_LITE_LIBRARIES})

TARGET_LINK_LIBRARIES(RawChanges_test PRIVATE
  ${Boost_UNIT_TEST_FRAMEWORK_LIBRARY})

TARGET_LINK_LIBRARIES(ElectionHandler_test PRIVATE
  lluf_utils
  lluf_id
  ${Boost_UNIT_TEST_FRAMEWORK_LIBRARY}
  ${PROTOBUF_LITE_LIBRARIES})

TARGET_LINK_LIBRARIES(Coordinator_test PRIVATE
  lluf_utils
  ${Boost_UNIT_TEST_FRAMEWORK_LIBRARY}
  ${PROTOBUF_LITE_LIBRARIES})

add_test(NAME RawStatistics_test COMMAND RawStatistics_test)
SET_SAFIR_TEST_PROPERTIES(TEST RawStatistics_test TIMEOUT 30)

add_test(NAME SystemState_test COMMAND SystemState_test)
SET_SAFIR_TEST_PROPERTIES(TEST SystemState_test TIMEOUT 30)

add_test(NAME RawPublisherLocal_test COMMAND RawPublisherLocal_test)
SET_SAFIR_TEST_PROPERTIES(TEST RawPublisherLocal_test TIMEOUT 30)

add_test(NAME RawPublisherRemote_test COMMAND RawPublisherRemote_test)
SET_SAFIR_TEST_PROPERTIES(TEST RawPublisherRemote_test TIMEOUT 30)

add_test(NAME StatePublisherLocal_test COMMAND StatePublisherLocal_test)
SET_SAFIR_TEST_PROPERTIES(TEST StatePublisherLocal_test TIMEOUT 30)

add_test(NAME StatePublisherRemote_test COMMAND StatePublisherRemote_test)
SET_SAFIR_TEST_PROPERTIES(TEST StatePublisherRemote_test TIMEOUT 30)


add_test(NAME LocalSubscriber_test COMMAND LocalSubscriber_test)
SET_SAFIR_TEST_PROPERTIES(TEST LocalSubscriber_test TIMEOUT 30)

add_test(NAME RemoteSubscriber_test COMMAND RemoteSubscriber_test)
SET_SAFIR_TEST_PROPERTIES(TEST RemoteSubscriber_test TIMEOUT 30)

add_test(NAME RawHandler_test COMMAND RawHandler_test)
SET_SAFIR_TEST_PROPERTIES(TEST RawHandler_test TIMEOUT 30)

add_test(NAME RawChanges_test COMMAND RawChanges_test)
SET_SAFIR_TEST_PROPERTIES(TEST RawChanges_test TIMEOUT 30)

add_test(NAME ElectionHandler_test COMMAND ElectionHandler_test)
SET_SAFIR_TEST_PROPERTIES(TEST ElectionHandler_test TIMEOUT 1200)

add_test(NAME ElectionHandler_test_with_overflows COMMAND ElectionHandler_test --overflows)
SET_SAFIR_TEST_PROPERTIES(TEST ElectionHandler_test_with_overflows TIMEOUT 1200)

add_test(NAME Coordinator_test COMMAND Coordinator_test)
SET_SAFIR_TEST_PROPERTIES(TEST Coordinator_test TIMEOUT 30)
