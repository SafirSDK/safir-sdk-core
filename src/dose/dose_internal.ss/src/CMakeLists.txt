FILE(GLOB_RECURSE headers *.h)
FILE(GLOB sources *.cpp)

ADD_LIBRARY(dose_internal SHARED ${sources} ${headers})

#Make our INTERNAL headers available to other parts of Core.
target_include_directories(dose_internal PUBLIC $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}/include>)

TARGET_LINK_LIBRARIES(dose_internal PRIVATE
  dots_cpp
  lluf_startup_synchronizer
  ${Boost_REGEX_LIBRARY}
  ${Boost_THREAD_LIBRARY}
  ${Boost_SYSTEM_LIBRARY}

  PUBLIC safir_generated-Core-cpp)

if (UNIX)
  TARGET_LINK_LIBRARIES(dose_internal PRIVATE rt)
endif()


SAFIR_INSTALL(TARGETS dose_internal)
