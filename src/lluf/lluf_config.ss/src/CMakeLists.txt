FILE(GLOB_RECURSE headers *.h)
FILE(GLOB sources *.cpp)

ADD_LIBRARY(lluf_config SHARED ${sources} ${headers})

#Make our INTERNAL headers available to other parts of Core.
target_include_directories(lluf_config PUBLIC $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}/include>)

TARGET_LINK_LIBRARIES (lluf_config PRIVATE)

SAFIR_INSTALL(
  TARGETS lluf_config)
