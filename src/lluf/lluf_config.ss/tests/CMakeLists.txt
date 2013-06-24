cmake_minimum_required(VERSION 2.8)

project(lluf_config_test CXX)

INCLUDE($ENV{SAFIR_SDK}/data/build/cmake_settings.txt)

ENABLE_TESTING()

ADD_EXECUTABLE(read_parameter_test read_parameter_test.cpp)
ADD_EXECUTABLE(Path_test Path_test.cpp)
IF (NOT MSVC)
  TARGET_LINK_LIBRARIES (read_parameter_test lluf_config)
  TARGET_LINK_LIBRARIES (Path_test lluf_config)
ENDIF()

ADD_TEST(ConfigReader_read_parameter read_parameter_test)
ADD_TEST(ConfigReader_Path Path_test ${lluf_config_test_SOURCE_DIR} ${lluf_config_test_SOURCE_DIR}/Path_test.cpp)
