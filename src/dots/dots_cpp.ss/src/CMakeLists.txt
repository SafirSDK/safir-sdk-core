FILE(GLOB_RECURSE headers *.h)
FILE(GLOB sources *.cpp)

ADD_LIBRARY(dots_cpp SHARED ${sources} ${headers})
safir_mark_dual_abi(dots_cpp)

#Make our INTERNAL headers available to other parts of Core.
target_include_directories(dots_cpp PUBLIC $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}/include>)

TARGET_LINK_LIBRARIES(dots_cpp PRIVATE
  lluf_id
  lluf_internal
  lluf_utils
  dots_kernel
  lluf_crash_reporter
  Boost::thread
)
SAFIR_INSTALL(TARGETS dots_cpp)
