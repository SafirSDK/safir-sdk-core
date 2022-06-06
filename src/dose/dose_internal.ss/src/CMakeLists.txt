FILE(GLOB_RECURSE headers *.h)
FILE(GLOB sources *.cpp)

ADD_LIBRARY(dose_internal SHARED ${sources} ${headers})

#Make our INTERNAL headers available to other parts of Core.
target_include_directories(dose_internal PUBLIC $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}/include>)

TARGET_LINK_LIBRARIES(dose_internal PRIVATE
  lluf_id
  lluf_internal
  lluf_utils
  Boost::regex

  PUBLIC
  $<BUILD_INTERFACE:lluf_startup_synchronizer>
  $<BUILD_INTERFACE:safir_generated-Core-cpp>
  $<BUILD_INTERFACE:Boost::thread>)

if (UNIX)
  TARGET_LINK_LIBRARIES(dose_internal PRIVATE rt)
endif()



SAFIR_INSTALL(TARGETS dose_internal)
