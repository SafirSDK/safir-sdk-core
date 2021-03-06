#The tests in here need the generated protobuf headers
#all the control_utils headers
INCLUDE_DIRECTORIES(${CMAKE_CURRENT_BINARY_DIR} ../../src/include)

ADD_SUBDIRECTORY(Config_test)

PROTOBUF_GENERATE_CPP(PROTO_SRCS PROTO_HDRS
  ../../src/DoseMainCommands.proto ../../src/ControlCommands.proto ../../src/ControlInfo.proto)

if (MSVC)
  SET_SOURCE_FILES_PROPERTIES(${PROTO_SRCS} PROPERTIES COMPILE_FLAGS "/wd4127 /wd4244 /wd4267 /wd4018 /wd4512")
endif()

#Need to define this to not get inconsistent dll linkage on windows.
#no test is meant to actually link the control_utils library, so we can do this.
add_definitions(-Dcontrol_utils_EXPORTS)

ADD_EXECUTABLE(DoseMainCmd_test DoseMainCmd_test.cpp ../../src/DoseMainCmd.cpp ${PROTO_SRCS} ${PROTO_HDRS})
ADD_EXECUTABLE(ControlCmd_test ControlCmd_test.cpp ../../src/ControlCmd.cpp ${PROTO_SRCS} ${PROTO_HDRS})
ADD_EXECUTABLE(ControlInfo_test ControlInfo_test.cpp ../../src/ControlInfo.cpp ${PROTO_SRCS} ${PROTO_HDRS})

TARGET_LINK_LIBRARIES(DoseMainCmd_test PRIVATE
  control_utils
  lluf_utils
  ${Boost_UNIT_TEST_FRAMEWORK_LIBRARY}
  ${PROTOBUF_LITE_LIBRARIES})

TARGET_LINK_LIBRARIES(ControlCmd_test PRIVATE
  control_utils
  lluf_utils
  ${Boost_UNIT_TEST_FRAMEWORK_LIBRARY}
  ${PROTOBUF_LITE_LIBRARIES})

TARGET_LINK_LIBRARIES(ControlInfo_test PRIVATE
  control_utils
  lluf_utils
  ${Boost_UNIT_TEST_FRAMEWORK_LIBRARY}
  ${PROTOBUF_LITE_LIBRARIES})

ADD_TEST(NAME DoseMainCmd_test COMMAND DoseMainCmd_test)
SET_SAFIR_TEST_PROPERTIES(TEST DoseMainCmd_test TIMEOUT 240)

ADD_TEST(NAME ControlCmd_test COMMAND ControlCmd_test)
SET_SAFIR_TEST_PROPERTIES(TEST ControlCmd_test TIMEOUT 240)

ADD_TEST(NAME ControlInfo_test COMMAND ControlInfo_test)
SET_SAFIR_TEST_PROPERTIES(TEST ControlInfo_test TIMEOUT 240)

