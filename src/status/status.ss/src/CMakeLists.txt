set(headers
  StatusApp.h StatusEntityHandler.h CommandRequestHandler.h)

set(sources
  StatusApp.cpp safir_status.cpp StatusEntityHandler.cpp CommandRequestHandler.cpp)

ADD_EXECUTABLE(safir_status ${sources} ${headers})

TARGET_LINK_LIBRARIES(safir_status PRIVATE
  control_utils
  dose_cpp
  lluf_internal
  lluf_crash_reporter)

SAFIR_INSTALL(TARGETS safir_status)
