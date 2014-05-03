FILE(GLOB_RECURSE headers *.h)
FILE(GLOB sources *.cpp)

ADD_LIBRARY(dots_cpp SHARED ${sources} ${headers})

target_include_directories(dots_cpp PUBLIC .)

TARGET_LINK_LIBRARIES(dots_cpp PRIVATE 
  lluf_config 
  lluf_id
  lluf_utils
  lluf_crash_reporter
  dots_internal
  dots_kernel
  ${Boost_THREAD_LIBRARY}
  ${Boost_SYSTEM_LIBRARY})

SAFIR_INSTALL(TARGETS dots_cpp INCLUDE_DIR Safir)

# INSTALL_DEBUG_INFO(dots_cpp)
