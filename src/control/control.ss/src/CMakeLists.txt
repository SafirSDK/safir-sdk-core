#first we try to see if boost.process is already installed on the computer
#if it is not we will have to download our own copy of it.
FIND_PATH(BOOST_PROCESS_INCLUDE_DIRECTORY
  NAMES
    boost/process.hpp
  PATHS
    /usr/include
    /usr/local/include)

if (BOOST_PROCESS_INCLUDE_DIRECTORY)
  MESSAGE(STATUS "Boost.Process seems to be installed on this computer, will use the installed version.")
  set(BOOST_PROCESS_FOUND True)
else()
  MESSAGE(STATUS "Boost.Process is not installed on this computer, will download and build it.")
  set(BOOST_PROCESS_FOUND False)

  ADD_SUBDIRECTORY(boost-process)
  set(BOOST_PROCESS_INCLUDE_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/boost-process/src)
endif()

set(headers ControlApp.h SystemStateHandler.h IncarnationBlacklistHandler.h)
set(sources safir_control.cpp ControlApp.cpp)

ADD_EXECUTABLE(safir_control ${sources} ${headers})

TARGET_LINK_LIBRARIES(safir_control PRIVATE
  system_picture
  control_utils
  lluf_utils
  lluf_crash_reporter
  ${Boost_FILESYSTEM_LIBRARY}
  ${Boost_PROGRAM_OPTIONS_LIBRARY})

target_include_directories(safir_control PRIVATE ${BOOST_PROCESS_INCLUDE_DIRECTORY})

if (NOT BOOST_PROCESS_FOUND)
  add_dependencies(safir_control boost-process)
endif()



SAFIR_INSTALL(TARGETS safir_control)
