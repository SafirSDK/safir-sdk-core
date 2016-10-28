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

if (MSVC)
  ExternalProject_Add(
    google-breakpad
    PREFIX ""
    SOURCE_DIR ${CMAKE_CURRENT_SOURCE_DIR}/breakpad
    BINARY_DIR bin
    PATCH_COMMAND ${CMAKE_COMMAND} -E copy ${CMAKE_CURRENT_SOURCE_DIR}/build-breakpad-windows.cmake <SOURCE_DIR>/CMakeLists.txt
    CMAKE_ARGS -DCMAKE_BUILD_TYPE:STRING=${CMAKE_BUILD_TYPE}
    INSTALL_COMMAND ""
    )
else()
  ExternalProject_Add(
    google-breakpad
    PREFIX ""
    SOURCE_DIR ${CMAKE_CURRENT_SOURCE_DIR}/breakpad
    BINARY_DIR ${CMAKE_CURRENT_SOURCE_DIR}/breakpad
    PATCH_COMMAND ${CMAKE_COMMAND} -E create_symlink <SOURCE_DIR>/../linux-syscall-support <SOURCE_DIR>/src/third_party/lss
    CONFIGURE_COMMAND cd <SOURCE_DIR> && ./configure
    BUILD_COMMAND cd <SOURCE_DIR> && make src/client/linux/libbreakpad_client.a
    INSTALL_COMMAND ""
    )
endif()
