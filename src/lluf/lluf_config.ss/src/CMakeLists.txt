FILE(GLOB_RECURSE headers *.h)
FILE(GLOB sources *.cpp)

ADD_LIBRARY(lluf_config SHARED ${sources} ${headers})

TARGET_LINK_LIBRARIES (lluf_config PRIVATE
  ${Boost_SYSTEM_LIBRARY})

SAFIR_INSTALL(
  TARGETS lluf_config)
