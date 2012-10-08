#cmake_minimum_required(VERSION 2.8)
#INCLUDE($ENV{SAFIR_SDK}/data/build/cmake_settings.txt)

include(ExternalProject)

set(MY_SVN_REPOSITORY http://google-breakpad.googlecode.com/svn/trunk)
set(MY_SVN_REVISION 1064)

if (MSVC)
  ExternalProject_Add(
    google-breakpad
    PREFIX ""
    SOURCE_DIR src
    BINARY_DIR bin
    SVN_REPOSITORY ${MY_SVN_REPOSITORY}
    SVN_REVISION -r ${MY_SVN_REVISION}
    UPDATE_COMMAND "" #disable update since we're going against fixed revision
    CONFIGURE_COMMAND ""
    BUILD_COMMAND ""
    INSTALL_COMMAND "")
else()
  ExternalProject_Add(
    google-breakpad
    PREFIX ""
    SOURCE_DIR src
    BINARY_DIR bin
    SVN_REPOSITORY ${MY_SVN_REPOSITORY}
    SVN_REVISION -r ${MY_SVN_REVISION}
    UPDATE_COMMAND "" #disable update since we're going against fixed revision
    CONFIGURE_COMMAND <SOURCE_DIR>/configure CFLAGS=-fPIC CXXFLAGS=-fPIC
    INSTALL_COMMAND ""
    ) 
endif()

if (MSVC)
  FILE(GLOB headers 
    ./src/src/client/windows/handler/*.h 
    ./src/src/client/windows/crash_generation/crash_generation_client.h 
    ./src/src/common/windows/guid_string.h)  
  FILE(GLOB sources 
    ./src/src/client/windows/handler/*.cc 
    ./src/src/client/windows/crash_generation/crash_generation_client.cc 
    ./src/src/common/windows/guid_string.cc)
  
  add_library(breakpad_client STATIC  ${headers} ${sources})
  
  add_definitions(-DUNICODE -D_UNICODE)
  add_definitions(/wd4127 /wd4245)
endif()
