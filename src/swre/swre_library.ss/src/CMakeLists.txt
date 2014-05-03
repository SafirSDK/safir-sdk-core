#this library has a C interface and we only want one version of it,
#so we never want the "d" suffix on debug builds, which is default on windows.
SET(CMAKE_DEBUG_POSTFIX "")

FILE(GLOB_RECURSE headers *.h)
FILE(GLOB sources *.cpp)

ADD_LIBRARY(swre_library SHARED ${sources} ${headers})

target_include_directories(swre_library PUBLIC .)

TARGET_LINK_LIBRARIES(swre_library PRIVATE 
  douf_time_cpp 
  dose_internal
  dose_cpp
  dots_generated-swre_library-cpp
  logging_cpp
  lluf_utils
  lluf_crash_reporter
  lluf_config
  ${Boost_REGEX_LIBRARY}
  ${Boost_THREAD_LIBRARY}
  ${Boost_SYSTEM_LIBRARY})

SAFIR_INSTALL(TARGETS swre_library INCLUDE_DIR Safir)

# INSTALL(FILES ../data/swre_library.gpr
#         DESTINATION ${SAFIR_SDK}/ada)

