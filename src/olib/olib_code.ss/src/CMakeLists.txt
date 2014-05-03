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

SAFIR_INSTALL(TARGETS olib INCLUDE_DIR Safir)
