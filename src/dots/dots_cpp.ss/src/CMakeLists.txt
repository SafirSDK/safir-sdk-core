FILE(GLOB_RECURSE headers *.h)
FILE(GLOB sources *.cpp)

ADD_LIBRARY(dots_cpp SHARED ${sources} ${headers})

#Make our INTERNAL headers available to other parts of Core.
target_include_directories(dots_cpp PUBLIC $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}/include>)

TARGET_LINK_LIBRARIES(dots_cpp PRIVATE
  lluf_id
  lluf_utils
  dots_kernel
  lluf_crash_reporter
  
  PUBLIC
  ${Boost_THREAD_LIBRARY}
  ${Boost_SYSTEM_LIBRARY}
)
SAFIR_INSTALL(TARGETS dots_cpp)
