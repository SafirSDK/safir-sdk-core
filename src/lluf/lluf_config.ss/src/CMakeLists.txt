FILE(GLOB_RECURSE headers *.h)
FILE(GLOB sources *.cpp)

ADD_LIBRARY(lluf_config SHARED ${sources} ${headers})

#Make our INTERNAL headers available to other parts of Core.
TARGET_INCLUDE_DIRECTORIES(lluf_config PUBLIC $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}/include>)

SAFIR_INSTALL(
  TARGETS lluf_config)
