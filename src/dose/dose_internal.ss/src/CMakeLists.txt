FILE(GLOB_RECURSE headers *.h)
FILE(GLOB sources *.cpp)

ADD_LIBRARY(dose_internal SHARED ${sources} ${headers})

#Make our INTERNAL headers available to other parts of Core.
target_include_directories(dose_internal PUBLIC $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}/include>)

TARGET_LINK_LIBRARIES(dose_internal PRIVATE
  dots_cpp 
  ${Boost_REGEX_LIBRARY}
  ${Boost_THREAD_LIBRARY}
  ${Boost_SYSTEM_LIBRARY}

  PUBLIC dots_generated-Safir-cpp)

SAFIR_INSTALL(TARGETS dose_internal)
