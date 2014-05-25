#this library has a C interface and we only want one version of it,
#so we never want the "d" suffix on debug builds, which is default on windows.
SET(CMAKE_DEBUG_POSTFIX "")

FILE(GLOB_RECURSE headers *.h)
FILE(GLOB sources *.cpp)

ADD_LIBRARY(dots_kernel SHARED ${sources} ${headers})

target_include_directories(dots_kernel PUBLIC .)

TARGET_LINK_LIBRARIES(dots_kernel PRIVATE 
  lluf_config
  lluf_startup_synchronizer
  lluf_crash_reporter
  lluf_utils
  lluf_id
  dots_internal
  ${Boost_FILESYSTEM_LIBRARY}
  ${Boost_THREAD_LIBRARY}
  ${Boost_SYSTEM_LIBRARY})

SAFIR_INSTALL(TARGETS dots_kernel INCLUDE_DIR Safir)

#TODO
# INSTALL(FILES ../data/dots_kernel.gpr
#         DESTINATION ${SAFIR_SDK}/ada/)
