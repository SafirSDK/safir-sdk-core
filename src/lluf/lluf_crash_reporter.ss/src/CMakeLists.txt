cmake_minimum_required(VERSION 2.6)
project(lluf_crash_reporter C CXX)

#this library has C++ interface that *should* work in both debug and release
#so we don't want the "d" suffix on debug builds on windows
#Must be set before including cmake_settings.txt
SET(NO_LIBRARY_POSTFIXES 1) 

INCLUDE($ENV{SAFIR_SDK}/data/build/cmake_settings.txt)

INCLUDE_DIRECTORIES(./) #Google breakpad source dir

if (MSVC)
  FILE(GLOB headers ./client/windows/handler/*.h ./client/windows/crash_generation/crash_generation_client.h ./common/windows/guid_string.h)
  FILE(GLOB sources ./client/windows/handler/*.cc ./client/windows/crash_generation/crash_generation_client.cc ./common/windows/guid_string.cc)
else()
  FILE(GLOB headers   
    ./client/linux/handler/*.h
    ./client/linux/crash_generation/*.h
    ./client/linux/minidump_writer/*.h
    ./client/linux/log/*.h
    ./client/*.h
    ./common/linux/*.h
    ./common/dwarf/*.h
    ./common/*.h
    #./processor/*.h
    )
  FILE(GLOB sources   
    ./client/linux/handler/exception_handler.cc
    ./client/linux/crash_generation/*.cc
    ./client/linux/minidump_writer/*.cc
    ./client/linux/log/*.cc
    ./client/*.cc
    ./common/linux/*.cc
    ./common/dwarf/*.cc
    ./common/*.cc
    ./common/*.c
    #./processor/*.cc
    )
endif()

add_library(breakpad SHARED  ${headers} ${sources})

add_definitions(-DUNICODE -D_UNICODE)

ADD_DEFINITIONS(-DBREAKPAD_EXPORTS)

SET_TARGET_PROPERTIES(breakpad PROPERTIES LINKER_LANGUAGE CXX)

INSTALL(DIRECTORY ./client DESTINATION ${SAFIR_SDK}/include/Safir/Utilities/Breakpad
  FILES_MATCHING PATTERN "*.h")

INSTALL(DIRECTORY ./common DESTINATION ${SAFIR_SDK}/include/Safir/Utilities/Breakpad
  FILES_MATCHING PATTERN "*.h")

INSTALL(DIRECTORY ./google_breakpad DESTINATION ${SAFIR_SDK}/include/Safir/Utilities/Breakpad
  FILES_MATCHING PATTERN "*.h")

INSTALL(DIRECTORY ./processor DESTINATION ${SAFIR_SDK}/include/Safir/Utilities/Breakpad
  FILES_MATCHING PATTERN "*.h")

INSTALL(DIRECTORY ./third_party DESTINATION ${SAFIR_SDK}/include/Safir/Utilities/Breakpad
  FILES_MATCHING PATTERN "*.h")

if (MSVC)
  INSTALL(DIRECTORY ./client/windows/handler/ DESTINATION ${SAFIR_SDK}/include/Safir/Utilities/Breakpad
    FILES_MATCHING PATTERN "*.h")
else()
  INSTALL(DIRECTORY ./client/linux/handler/ DESTINATION ${SAFIR_SDK}/include/Safir/Utilities/Breakpad
    FILES_MATCHING PATTERN "*.h")
endif()

INSTALL(TARGETS breakpad 
  RUNTIME DESTINATION ${SAFIR_RUNTIME}/bin
  LIBRARY DESTINATION ${SAFIR_RUNTIME}/lib
  ARCHIVE DESTINATION ${SAFIR_SDK}/lib)

INSTALL_DEBUG_INFO(breakpad)
