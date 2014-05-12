FILE(GLOB_RECURSE headers *.h)
FILE(GLOB sources *.cpp)

ADD_LIBRARY(olib SHARED ${sources} ${headers})

target_include_directories(olib PUBLIC .)

TARGET_LINK_LIBRARIES(olib PRIVATE 
  douf_time_cpp 
  dots_generated-olib-cpp
  logging_cpp
  ${SAFIR_ODBC_LIBRARIES})

SAFIR_INSTALL(TARGETS olib INCLUDE_DIR Safir)
