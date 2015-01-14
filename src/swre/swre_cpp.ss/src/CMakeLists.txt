ADD_LIBRARY(swre_interface_cpp SHARED BackdoorKeeper.cpp CrashReporter.cpp SwReport.cpp Tracer.cpp TraceStreamBuffer.cpp)

TARGET_LINK_LIBRARIES(swre_interface_cpp PRIVATE
  swre_library
  logging_cpp
  ${Boost_REGEX_LIBRARY}

  PUBLIC
  dose_cpp
  safir_generated-Core-cpp)

SAFIR_INSTALL(TARGETS swre_interface_cpp)
