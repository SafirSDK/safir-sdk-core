

ADD_EXECUTABLE(read_parameter_test read_parameter_test.cpp)
ADD_EXECUTABLE(Path_test Path_test.cpp)
ADD_EXECUTABLE(Util_test Util_test.cpp)
ADD_EXECUTABLE(PathFinders_test PathFinders_test.cpp)
ADD_EXECUTABLE(ConfigReaderImpl_test ConfigReaderImpl_test.cpp)
ADD_EXECUTABLE(ConfigHelper_test ConfigHelper_test.cpp)
TARGET_LINK_LIBRARIES (read_parameter_test lluf_config)
TARGET_LINK_LIBRARIES (Path_test lluf_config)
TARGET_LINK_LIBRARIES (Util_test lluf_config)
TARGET_LINK_LIBRARIES (PathFinders_test lluf_config)
TARGET_LINK_LIBRARIES (ConfigReaderImpl_test lluf_config)
TARGET_LINK_LIBRARIES (ConfigHelper_test lluf_config)

ADD_TEST(ConfigReader_read_parameter read_parameter_test)
SET_SAFIR_TEST_PROPERTIES(TEST ConfigReader_read_parameter)

ADD_TEST(ConfigReader_Path Path_test ${CMAKE_CURRENT_SOURCE_DIR} ${CMAKE_CURRENT_SOURCE_DIR}/Path_test.cpp)
ADD_TEST(ConfigReader_Util Util_test)
ADD_TEST(ConfigReader_PathFinders PathFinders_test)
ADD_TEST(ConfigReader_ConfigReaderImpl ConfigReaderImpl_test ${CMAKE_CURRENT_SOURCE_DIR}/testconf)

ADD_TEST(ConfigReader_ConfigHelper ConfigHelper_test)
SET_SAFIR_TEST_PROPERTIES(TEST ConfigReader_ConfigHelper 
  CONFIG_OVERRIDE ${CMAKE_CURRENT_SOURCE_DIR}/testconf/config_helper/)

