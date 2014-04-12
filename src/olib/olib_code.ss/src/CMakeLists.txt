if (UNIX)
    FIND_PACKAGE(IODBC)
    if (IODBC_FOUND)
      INCLUDE_DIRECTORIES(${IODBC_INCLUDE_DIRS})
      SET(ODBC_LIBRARIES ${IODBC_LIBRARIES})
      ADD_DEFINITIONS(-DODBC_IS_IODBC)
    else()
      message("iODBC not found, trying UnixODBC")
      FIND_PACKAGE(UnixODBC REQUIRED)
      INCLUDE_DIRECTORIES(${UnixODBC_INCLUDE_DIRS})
      SET(ODBC_LIBRARIES ${UnixODBC_LIBRARIES})
      ADD_DEFINITIONS(-DODBC_IS_UNIXODBC)
    endif()
endif()


FILE(GLOB_RECURSE headers *.h)
FILE(GLOB sources *.cpp)

ADD_LIBRARY(olib SHARED ${sources} ${headers})

target_include_directories(olib PUBLIC .)

TARGET_LINK_LIBRARIES(olib PRIVATE 
  douf_time_cpp 
  dots_generated-olib-cpp
  logging_cpp ${ODBC_LIBRARIES})

INSTALL(DIRECTORY Safir DESTINATION ${SAFIR_SDK}/include
        PATTERN ".svn" EXCLUDE
        PATTERN "*~" EXCLUDE)

INSTALL(TARGETS olib
        RUNTIME DESTINATION ${SAFIR_RUNTIME}/bin
        LIBRARY DESTINATION ${SAFIR_RUNTIME}/lib
        ARCHIVE DESTINATION ${SAFIR_SDK}/lib)

INSTALL_DEBUG_INFO(olib)
