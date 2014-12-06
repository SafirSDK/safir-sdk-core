#We use the cmake ExternalProject functionality to fetch and build google breakpad.
#It fetches the desired revision from the breakpad svn repository.
#Just change MY_SVN_REPOSITORY below to change the fetched revision.
#ExternalProject also supports fetching a zip from an url, if that becomes preferable
#at some stage. See http://www.kitware.com/media/html/BuildingExternalProjectsWithCMake2.8.html
#
#We have different build code for windows and linux:
#Linux: build using configure & make, but do not install, instead the lluf_crash_reporter
#library includes and links directly into the source/build directories in here.
#Windows: Breakpad uses gyp to generate its project files. Unfortunately gyp is python2 only,
#and we support python3-only environments. So instead we patch a cmake file on to the breakpad
#source and use that to build on windows.
include(ExternalProject)

set(MY_SVN_REPOSITORY http://google-breakpad.googlecode.com/svn/trunk)
set(MY_SVN_REVISION 1408)
set(TARBALL_MD5 ef2824b22e75d18951e9040d1c5df9b3)

set(CACHE_URL http://safir-jenkins-master:8080/safir/job/Breakpad%20cache/lastSuccessfulBuild/artifact/google-breakpad-${MY_SVN_REVISION}.tar.gz)

file (DOWNLOAD ${CACHE_URL}
  ${CMAKE_CURRENT_BINARY_DIR}/google-breakpad.tar.gz
  STATUS download_result
  )
list (GET download_result 0 download_result_0)

if (NOT download_result_0 EQUAL 0)
  message(STATUS "Failed to download cached tarball: [${download_result}]")
else()
  file(MD5 ${CMAKE_CURRENT_BINARY_DIR}/google-breakpad.tar.gz md5)
  if (NOT md5 STREQUAL TARBALL_MD5)
    message(STATUS "MD5 mismatch. Expected ${TARBALL_MD5}")
    message(STATUS "                   got ${md5}")
  endif()
endif()

if (download_result EQUAL 0 AND md5 STREQUAL TARBALL_MD5)
  message(STATUS "Found cached breakpad tarball.")
  set (DOWNLOAD_COMMANDS
    URL ${CMAKE_CURRENT_BINARY_DIR}/google-breakpad.tar.gz)
else()
  message(STATUS "No cached breakpad tarball found will use svn.")
  set (DOWNLOAD_COMMANDS
    SVN_REPOSITORY ${MY_SVN_REPOSITORY}
    SVN_REVISION -r ${MY_SVN_REVISION}
    UPDATE_COMMAND "" #disable update since we're going against fixed revision
    )
endif()

if (MSVC)
  ExternalProject_Add(
    google-breakpad
    PREFIX ""
    SOURCE_DIR src
    BINARY_DIR bin
    ${DOWNLOAD_COMMANDS}
    PATCH_COMMAND ${CMAKE_COMMAND} -E copy ${CMAKE_CURRENT_SOURCE_DIR}/build-breakpad-windows.cmake <SOURCE_DIR>/CMakeLists.txt
    CMAKE_ARGS -DCMAKE_BUILD_TYPE:STRING=${CMAKE_BUILD_TYPE}
    INSTALL_COMMAND ""
    )
else()
  ExternalProject_Add(
    google-breakpad
    PREFIX ""
    SOURCE_DIR src
    BINARY_DIR src
    ${DOWNLOAD_COMMANDS}
    CONFIGURE_COMMAND cd <SOURCE_DIR> && ./configure CFLAGS=-fPIC CXXFLAGS=-fPIC
    INSTALL_COMMAND ""
    )
endif()
