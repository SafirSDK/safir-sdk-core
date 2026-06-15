FILE(GLOB sources *.cpp)

#we need to disable some warnings, since we do some wierd things with pointers
if (NOT MSVC)
  SET(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -Wno-pedantic")
endif()


ADD_LIBRARY(douf_time_library SHARED ${sources})
safir_mark_single_abi(douf_time_library)

#Make our INTERNAL headers available to other parts of Core.
target_include_directories(douf_time_library PUBLIC $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}/include>)

TARGET_LINK_LIBRARIES(douf_time_library PRIVATE
  safir_generated-Core-cpp
  lluf_internal
  lluf_utils)

SAFIR_INSTALL(TARGETS douf_time_library)

