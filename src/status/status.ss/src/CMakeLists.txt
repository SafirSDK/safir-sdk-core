FILE(GLOB headers
  StatusApp.h StatusEntityHandler.h)

FILE(GLOB sources
  StatusApp.cpp safir_status.cpp StatusEntityHandler.cpp)

ADD_EXECUTABLE(safir_status ${sources} ${headers})

TARGET_LINK_LIBRARIES(safir_status PRIVATE
  dose_cpp
  lluf_utils
  lluf_crash_reporter)

SAFIR_INSTALL(TARGETS safir_status)
