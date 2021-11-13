FILE(GLOB_RECURSE headers *.h)
FILE(GLOB sources *.cpp)

ADD_LIBRARY(lluf_config SHARED ${sources} ${headers})

TARGET_LINK_LIBRARIES (lluf_config PRIVATE
  Boost::system)

SAFIR_INSTALL(
  TARGETS lluf_config)
