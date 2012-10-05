cmake_minimum_required(VERSION 2.8)

project(lluf_crash_reporter CXX)

INCLUDE($ENV{SAFIR_SDK}/data/build/cmake_settings.txt)

INCLUDE_DIRECTORIES(. google-breakpad/src/)
LINK_DIRECTORIES(${CMAKE_CURRENT_SOURCE_DIR}/google-breakpad/src/)

add_library(lluf_crash_reporter SHARED CrashReporter.cpp Safir/Utilities/CrashReporter.h)

FIND_PACKAGE(Boost COMPONENTS filesystem thread system)

TARGET_LINK_LIBRARIES(lluf_crash_reporter breakpad)

if (NOT MSVC)
  TARGET_LINK_LIBRARIES(lluf_crash_reporter ${Boost_LIBRARIES})
endif()

INSTALL(DIRECTORY Safir DESTINATION ${SAFIR_SDK}/include
        PATTERN ".svn" EXCLUDE
        PATTERN "*~" EXCLUDE)

INSTALL(TARGETS lluf_crash_reporter
        RUNTIME DESTINATION ${SAFIR_RUNTIME}/bin
        LIBRARY DESTINATION ${SAFIR_RUNTIME}/lib
        ARCHIVE DESTINATION ${SAFIR_SDK}/lib)

INSTALL_DEBUG_INFO(lluf_crash_reporter)
