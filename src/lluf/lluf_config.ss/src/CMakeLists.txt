FILE(GLOB_RECURSE headers *.h)
FILE(GLOB sources *.cpp)

ADD_LIBRARY(lluf_config SHARED ${sources} ${headers})

TARGET_LINK_LIBRARIES (lluf_config PRIVATE)

SAFIR_INSTALL(
  TARGETS lluf_config)
