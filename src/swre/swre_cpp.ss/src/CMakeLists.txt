ADD_LIBRARY(swre_interface_cpp SHARED BackdoorKeeper.cpp CrashReporter.cpp SwReport.cpp Tracer.cpp TraceStreamBuffer.cpp)

TARGET_LINK_LIBRARIES(swre_interface_cpp PRIVATE
  swre_library
  logging_cpp
  
  PUBLIC
  dose_cpp
  dots_cpp)

SAFIR_INSTALL(TARGETS swre_interface_cpp)
