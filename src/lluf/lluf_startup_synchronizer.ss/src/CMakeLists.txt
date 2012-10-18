cmake_minimum_required(VERSION 2.8)

project(lluf_startup_synchronizer CXX)

#this library has an interface which is okay to call between debug/release runtimes,
#and we only want one version of it, so we don't want the "d" suffix on debug builds on windows.
#Must be set before including cmake_settings.txt
SET(NO_LIBRARY_POSTFIXES 1) 

INCLUDE_DIRECTORIES(.)

INCLUDE($ENV{SAFIR_SDK}/data/build/cmake_settings.txt)

add_library(lluf_startup_synchronizer SHARED StartupSynchronizer.cpp Safir/Utilities/StartupSynchronizer.h)

FIND_PACKAGE(Boost COMPONENTS filesystem thread system)

if (NOT MSVC)
  TARGET_LINK_LIBRARIES(lluf_startup_synchronizer ${Boost_LIBRARIES})
endif()

INSTALL(DIRECTORY Safir DESTINATION ${SAFIR_SDK}/include
        PATTERN ".svn" EXCLUDE
        PATTERN "*~" EXCLUDE)

INSTALL(TARGETS lluf_startup_synchronizer
        RUNTIME DESTINATION ${SAFIR_RUNTIME}/bin
        LIBRARY DESTINATION ${SAFIR_RUNTIME}/lib
        ARCHIVE DESTINATION ${SAFIR_SDK}/lib)

INSTALL_DEBUG_INFO(lluf_startup_synchronizer)
