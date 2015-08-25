FILE(GLOB headers
  StatusApp.h)

FILE(GLOB sources
  StatusApp.cpp status.cpp)

ADD_EXECUTABLE(status ${sources} ${headers})

TARGET_LINK_LIBRARIES(status PRIVATE
  lluf_utils
  lluf_crash_reporter)

SAFIR_INSTALL(TARGETS status)
