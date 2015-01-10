#TODO
INCLUDE_DIRECTORIES(${CMAKE_CURRENT_BINARY_DIR} ../../src/include)

ADD_SUBDIRECTORY(Config_test)


PROTOBUF_GENERATE_CPP(PROTO_SRCS PROTO_HDRS
  ../../src/ControlCommands.proto)

if (MSVC)
  SET_SOURCE_FILES_PROPERTIES(${PROTO_SRCS} PROPERTIES COMPILE_FLAGS "/wd4127 /wd4244 /wd4267")
endif()

#Need to define this to not get inconsistent dll linkage on windows.
#no test is meant to actually link the system_picture library, so we can do this.
add_definitions(-Dcontrol_utils_EXPORTS)

ADD_EXECUTABLE(DoseMainCmd_test DoseMainCmd_test.cpp ../../src/DoseMainCmd.cpp ${PROTO_SRCS} ${PROTO_HDRS})

TARGET_LINK_LIBRARIES(DoseMainCmd_test
  lluf_config
  lluf_utils
  ${Boost_UNIT_TEST_FRAMEWORK_LIBRARY}
  ${Boost_FILESYSTEM_LIBRARY}
  ${Boost_THREAD_LIBRARY}
  ${Boost_SYSTEM_LIBRARY}
  ${PROTOBUF_LITE_LIBRARIES})

ADD_TEST(DoseMainCmd_test DoseMainCmd_test)
SET_SAFIR_TEST_PROPERTIES(TEST DoseMainCmd_test TIMEOUT 240)

