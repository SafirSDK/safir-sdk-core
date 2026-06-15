ADD_LIBRARY(logging_cpp SHARED Log.cpp)
safir_mark_dual_abi(logging_cpp)

TARGET_LINK_LIBRARIES(logging_cpp PRIVATE logging_library lluf_internal)

SAFIR_INSTALL(TARGETS logging_cpp)
