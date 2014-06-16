#this library has a C interface and we only want one version of it,
#so we never want the "d" suffix on debug builds, which is default on windows.
SET(CMAKE_DEBUG_POSTFIX "")

FILE(GLOB sources *.cpp)

ADD_LIBRARY(douf_time_library SHARED ${sources})

#Make our INTERNAL headers available to other parts of Core.
target_include_directories(douf_time_library PUBLIC $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}/include>)

TARGET_LINK_LIBRARIES(douf_time_library PRIVATE 
  safir_generated-Core-cpp
  ${Boost_THREAD_LIBRARY}
  ${Boost_SYSTEM_LIBRARY})

SAFIR_INSTALL(TARGETS douf_time_library)

# INSTALL(FILES ../data/douf_time_library.gpr
#         DESTINATION ${SAFIR_SDK}/ada)

