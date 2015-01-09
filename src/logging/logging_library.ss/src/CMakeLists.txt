#this library has a C interface and we only want one version of it,
#so we never want the "d" suffix on debug builds, which is default on windows.
SET(CMAKE_DEBUG_POSTFIX "")

FILE(GLOB_RECURSE headers *.h)

ADD_LIBRARY(logging_library SHARED Interface.cpp include/Safir/Logging/Internal/Interface.h)

#Make our INTERNAL headers available to other parts of Core.
target_include_directories(logging_library PUBLIC $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}/include>)

TARGET_LINK_LIBRARIES(logging_library PRIVATE lluf_utils)

SAFIR_INSTALL(TARGETS logging_library)

# INSTALL(FILES ../data/logging_library.gpr
#         DESTINATION ${SAFIR_SDK}/ada)

