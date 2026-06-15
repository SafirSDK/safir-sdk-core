ADD_LIBRARY(logging_library SHARED Interface.cpp include/Safir/Logging/Internal/Interface.h)
safir_mark_single_abi(logging_library)

#Make our INTERNAL headers available to other parts of Core.
target_include_directories(logging_library PUBLIC $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}/include>)

TARGET_LINK_LIBRARIES(logging_library PRIVATE lluf_internal)

SAFIR_INSTALL(TARGETS logging_library)

