#this library has a C interface and we only want one version of it,
#so we never want the "d" suffix on debug builds, which is default on windows.
SET(CMAKE_DEBUG_POSTFIX "")

SET(headers Library.h Prefixes.h TracerDataSender.h
  include/Safir/SwReports/Internal/Interface.h )
SET(sources Interface.cpp Library.cpp)

ADD_LIBRARY(swre_library SHARED ${sources} ${headers})
ADD_EXECUTABLE(safir_tracer_listener tracer_listener.cpp)
#Make our INTERNAL headers available to other parts of Core.
target_include_directories(swre_library PUBLIC $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}/include>)

TARGET_LINK_LIBRARIES(swre_library PRIVATE
  control_utils
  communication
  dose_cpp
  logging_cpp
  lluf_internal
  lluf_utils
  lluf_crash_reporter
  lluf_config
)

TARGET_LINK_LIBRARIES(safir_tracer_listener PRIVATE
  Boost::program_options
  $<$<PLATFORM_ID:Windows>:iphlpapi>        # needed for GetAdaptersAddresses()
)

SAFIR_INSTALL(TARGETS swre_library)
SAFIR_INSTALL(TARGETS safir_tracer_listener COMPONENT Tools)
