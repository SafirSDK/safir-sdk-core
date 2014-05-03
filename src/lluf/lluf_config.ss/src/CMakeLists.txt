FILE(GLOB_RECURSE headers *.h)
FILE(GLOB sources *.cpp)

ADD_LIBRARY(lluf_config SHARED ${sources} ${headers})

TARGET_INCLUDE_DIRECTORIES(lluf_config PUBLIC .)

SAFIR_INSTALL(
  TARGETS lluf_config 
  INCLUDE_DIR Safir)
