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
    PATCH_COMMAND ${CMAKE_COMMAND} -E copy ${CMAKE_CURRENT_SOURCE_DIR}/build-breakpad-windows.cmake <SOURCE_DIR>/CMakeLists.txt
    UPDATE_COMMAND "" #disable update since we're going against fixed revision
    )
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

