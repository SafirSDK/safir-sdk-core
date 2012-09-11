cmake_minimum_required(VERSION 2.8)

project(lluf_crash_reporter C CXX)

#this library has C++ interface that *should* work in both debug and release
#so we don't want the "d" suffix on debug builds on windows
#Must be set before including cmake_settings.txt
SET(NO_LIBRARY_POSTFIXES 1) 

INCLUDE($ENV{SAFIR_SDK}/data/build/cmake_settings.txt)

INCLUDE_DIRECTORIES(. breakpad)

add_subdirectory(breakpad)

add_library(lluf_crash_reporter SHARED CrashReporter.cpp Safir/Utilities/CrashReporter.h)

FIND_PACKAGE(Boost COMPONENTS filesystem thread system)

if (NOT MSVC)
  TARGET_LINK_LIBRARIES(lluf_crash_reporter breakpad ${Boost_LIBRARIES})
endif()

INSTALL(DIRECTORY Safir DESTINATION ${SAFIR_SDK}/include
        PATTERN ".svn" EXCLUDE
        PATTERN "*~" EXCLUDE)

INSTALL(TARGETS lluf_crash_reporter
        RUNTIME DESTINATION ${SAFIR_RUNTIME}/bin
        LIBRARY DESTINATION ${SAFIR_RUNTIME}/lib
        ARCHIVE DESTINATION ${SAFIR_SDK}/lib)

INSTALL_DEBUG_INFO(lluf_crash_reporter)
