cmake_minimum_required(VERSION 2.8)

include(ExternalProject)

set(DOWNLOAD_URL http://www.highscore.de/boost/process0.5/process.zip)

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
    CMAKE_ARGS -DCMAKE_BUILD_TYPE:STRING=${CMAKE_BUILD_TYPE}
    CMAKE_GENERATOR "NMake Makefiles"
    )
else()
  ExternalProject_Add(
    boost-process
    PREFIX ""
    SOURCE_DIR src
    URL ${DOWNLOAD_URL}
    CONFIGURE_COMMAND ""
    UPDATE_COMMAND ""
    PATCH_COMMAND ""
    BUILD_COMMAND ""
    INSTALL_COMMAND ""
    
#    SOURCE_DIR src
#    BINARY_DIR src
#    UPDATE_COMMAND "" #disable update since we're going against fixed revision
#    CONFIGURE_COMMAND cd <SOURCE_DIR> && ./configure CFLAGS=-fPIC CXXFLAGS=-fPIC
#    INSTALL_COMMAND ""
    ) 
endif()

