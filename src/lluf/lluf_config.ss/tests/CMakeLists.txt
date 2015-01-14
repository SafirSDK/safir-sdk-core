
ADD_EXECUTABLE(display_env display_env.cpp)
ADD_EXECUTABLE(read_parameter_test read_parameter_test.cpp)
ADD_EXECUTABLE(Path_test Path_test.cpp)
ADD_EXECUTABLE(Util_test Util_test.cpp)
ADD_EXECUTABLE(PathFinders_test PathFinders_test.cpp)
ADD_EXECUTABLE(ConfigReaderImpl_test ConfigReaderImpl_test.cpp)
ADD_EXECUTABLE(ConfigHelper_test ConfigHelper_test.cpp)

TARGET_LINK_LIBRARIES (read_parameter_test PRIVATE lluf_config)
TARGET_LINK_LIBRARIES (Path_test PRIVATE lluf_config)
TARGET_LINK_LIBRARIES (Util_test PRIVATE lluf_config)
TARGET_LINK_LIBRARIES (PathFinders_test PRIVATE lluf_config)
TARGET_LINK_LIBRARIES (ConfigReaderImpl_test PRIVATE lluf_config)
TARGET_LINK_LIBRARIES (ConfigHelper_test PRIVATE lluf_config ${Boost_UNIT_TEST_FRAMEWORK_LIBRARY})

ADD_TEST(NAME display_test_env COMMAND display_env)
SET_SAFIR_TEST_PROPERTIES(TEST display_test_env)

ADD_TEST(NAME ConfigReader_read_parameter COMMAND read_parameter_test)
SET_SAFIR_TEST_PROPERTIES(TEST ConfigReader_read_parameter)

ADD_TEST(NAME ConfigReader_Path COMMAND Path_test ${CMAKE_CURRENT_SOURCE_DIR} ${CMAKE_CURRENT_SOURCE_DIR}/Path_test.cpp)
ADD_TEST(NAME ConfigReader_Util COMMAND Util_test)
ADD_TEST(NAME ConfigReader_PathFinders COMMAND PathFinders_test)
ADD_TEST(NAME ConfigReader_ConfigReaderImpl COMMAND ConfigReaderImpl_test ${CMAKE_CURRENT_SOURCE_DIR}/testconf)

file(MAKE_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/AnotherOverride)

ADD_TEST(NAME ConfigReader_ConfigHelper COMMAND ConfigHelper_test)
SET_SAFIR_TEST_PROPERTIES(TEST ConfigReader_ConfigHelper
  CONFIG_OVERRIDE ${CMAKE_CURRENT_SOURCE_DIR}/testconf/config_helper/)
