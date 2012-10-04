cmake_minimum_required(VERSION 2.8)

project(lluf_crash_reporter C CXX)

#this library has C++ interface that *should* work in both debug and release
#so we don't want the "d" suffix on debug builds on windows
#Must be set before including cmake_settings.txt
SET(NO_LIBRARY_POSTFIXES 1) 

INCLUDE($ENV{SAFIR_SDK}/data/build/cmake_settings.txt)

INCLUDE_DIRECTORIES(. external_breakpad)

if (MSVC)
  FILE(GLOB headers 
    ./external_breakpad/client/windows/handler/*.h 
    ./external_breakpad/client/windows/crash_generation/crash_generation_client.h 
    ./external_breakpad/common/windows/guid_string.h)
  FILE(GLOB sources 
    ./external_breakpad/client/windows/handler/*.cc 
    ./external_breakpad/client/windows/crash_generation/crash_generation_client.cc 
    ./external_breakpad/common/windows/guid_string.cc)
else()
  FILE(GLOB headers   
    ./external_breakpad/client/linux/handler/*.h
    ./external_breakpad/client/linux/crash_generation/*.h
    ./external_breakpad/client/linux/minidump_writer/*.h
    ./external_breakpad/client/linux/log/*.h
    ./external_breakpad/client/*.h
    ./external_breakpad/common/linux/*.h
    ./external_breakpad/common/dwarf/*.h
    ./external_breakpad/common/*.h
    )
  FILE(GLOB sources   
    ./external_breakpad/client/linux/handler/exception_handler.cc
    ./external_breakpad/client/linux/crash_generation/*.cc
    ./external_breakpad/client/linux/minidump_writer/*.cc
    ./external_breakpad/client/linux/log/*.cc
    ./external_breakpad/client/*.cc
    ./external_breakpad/common/linux/*.cc
    ./external_breakpad/common/dwarf/*.cc
    ./external_breakpad/common/*.cc
    ./external_breakpad/common/*.c
    )
endif()

add_library(lluf_crash_reporter SHARED CrashReporter.cpp Safir/Utilities/CrashReporter.h ${headers} ${sources})

FIND_PACKAGE(Boost COMPONENTS filesystem thread system)


if (NOT MSVC)
  TARGET_LINK_LIBRARIES(lluf_crash_reporter  ${Boost_LIBRARIES})
else(NOT MSVC)
  TARGET_LINK_LIBRARIES(lluf_crash_reporter   )
endif()

INSTALL(DIRECTORY Safir DESTINATION ${SAFIR_SDK}/include
        PATTERN ".svn" EXCLUDE
        PATTERN "*~" EXCLUDE)

INSTALL(TARGETS lluf_crash_reporter
        RUNTIME DESTINATION ${SAFIR_RUNTIME}/bin
        LIBRARY DESTINATION ${SAFIR_RUNTIME}/lib
        ARCHIVE DESTINATION ${SAFIR_SDK}/lib)

INSTALL_DEBUG_INFO(lluf_crash_reporter)
