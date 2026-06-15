FILE(GLOB_RECURSE headers *.h)
FILE(GLOB sources *.cpp)

ADD_LIBRARY(dose_dll SHARED ${sources} ${headers})
safir_mark_single_abi(dose_dll)

#Make our INTERNAL headers available to other parts of Core.
target_include_directories(dose_dll PUBLIC $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}/include>)

TARGET_LINK_LIBRARIES(dose_dll PRIVATE
  lluf_internal
  lluf_utils
  dose_internal)

SAFIR_INSTALL(TARGETS dose_dll)

