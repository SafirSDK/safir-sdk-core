cmake_minimum_required(VERSION 2.8)

project(lluf_config_test CXX)

INCLUDE($ENV{SAFIR_SDK}/data/build/cmake_settings.txt)

#FIND_PACKAGE(Boost COMPONENTS thread date_time system)

#FIND_PACKAGE(PythonInterp)

ENABLE_TESTING()

ADD_EXECUTABLE(config_reader_test test.cpp)
IF (NOT MSVC)
  TARGET_LINK_LIBRARIES (config_reader_test lluf_config)
ENDIF()

ADD_TEST(ConfigReader_test config_reader_test)
