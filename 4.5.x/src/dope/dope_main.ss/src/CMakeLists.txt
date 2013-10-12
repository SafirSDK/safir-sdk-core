cmake_minimum_required(VERSION 2.8)

project(dope_main CXX)

INCLUDE($ENV{SAFIR_SDK}/data/build/safir_sdk_core_config.cmake)

if (UNIX)
    FIND_PACKAGE(IODBC)
    if (IODBC_FOUND)
      INCLUDE_DIRECTORIES(${IODBC_INCLUDE_DIRS})
      SET(ODBC_LIBRARIES ${IODBC_LIBRARIES})
      message("iODBC not found, trying UnixODBC")
    else()
      FIND_PACKAGE(UnixODBC REQUIRED)
      INCLUDE_DIRECTORIES(${UnixODBC_INCLUDE_DIRS})
      SET(ODBC_LIBRARIES ${UnixODBC_LIBRARIES})
    endif()
endif()


ADD_SUBDIRECTORY(dope_main)
ADD_SUBDIRECTORY(dope_bin2xml)
