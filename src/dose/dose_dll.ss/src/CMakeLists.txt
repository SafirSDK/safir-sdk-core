#this library has a C interface and we only want one version of it,
#so we never want the "d" suffix on debug builds, which is default on windows.
SET(CMAKE_DEBUG_POSTFIX "")

FILE(GLOB_RECURSE headers *.h)
FILE(GLOB sources *.cpp)

ADD_LIBRARY(dose_dll SHARED ${sources} ${headers})

target_include_directories(dose_dll PUBLIC .)

#TODO: Move all public headers to a separate directory, in a "good way". Applies to all subsystems

TARGET_LINK_LIBRARIES(dose_dll PRIVATE 
  dose_internal
  dots_generated-Safir-cpp
  lluf_startup_synchronizer
  ${Boost_THREAD_LIBRARY}
  ${Boost_SYSTEM_LIBRARY})

SAFIR_INSTALL(TARGETS dose_dll INCLUDE_DIR Safir)

# INSTALL(FILES ../data/dose_dll.gpr
#         DESTINATION ${SAFIR_SDK}/ada)
