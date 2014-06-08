#this library has a C interface and we only want one version of it,
#so we never want the "d" suffix on debug builds, which is default on windows.
SET(CMAKE_DEBUG_POSTFIX "")

FILE(GLOB_RECURSE headers *.h)
FILE(GLOB sources *.cpp)

ADD_LIBRARY(swre_library SHARED ${sources} ${headers})

#Make our INTERNAL headers available to other parts of Core.
target_include_directories(swre_library PUBLIC $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}/include>)

TARGET_LINK_LIBRARIES(swre_library PRIVATE 
  dose_cpp
  dots_generated-swre_library-cpp
  logging_cpp
  ${Boost_REGEX_LIBRARY}
  ${Boost_THREAD_LIBRARY}
  ${Boost_SYSTEM_LIBRARY})

SAFIR_INSTALL(TARGETS swre_library)

# INSTALL(FILES ../data/swre_library.gpr
#         DESTINATION ${SAFIR_SDK}/ada)

