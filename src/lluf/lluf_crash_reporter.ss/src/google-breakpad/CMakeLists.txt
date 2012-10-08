#cmake_minimum_required(VERSION 2.8)
#INCLUDE($ENV{SAFIR_SDK}/data/build/cmake_settings.txt)

include(ExternalProject)

if (MSVC)
   set(MY_CONFIGURE_COMMAND "")
   set(MY_BUILD_COMMAND "")
else()
  set (MY_CONFIGURE_COMMAND <SOURCE_DIR>/configure CFLAGS=-fPIC CXXFLAGS=-fPIC)
  set(MY_BUILD_COMMAND make)
endif()

ExternalProject_Add(
   google-breakpad
   PREFIX ""
   SOURCE_DIR src
   BINARY_DIR bin
   SVN_REPOSITORY http://google-breakpad.googlecode.com/svn/trunk
   SVN_REVISION -r 1064
   UPDATE_COMMAND "" #disable update since we're going against fixed revision
   CONFIGURE_COMMAND ${MY_CONFIGURE_COMMAND}
   BUILD_COMMAND ${MY_BUILD_COMMAND}
   INSTALL_COMMAND ""
) 
