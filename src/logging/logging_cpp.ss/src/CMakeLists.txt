ADD_LIBRARY(logging_cpp SHARED Log.cpp)

TARGET_LINK_LIBRARIES(logging_cpp PRIVATE logging_library lluf_internal)

SAFIR_INSTALL(TARGETS logging_cpp)
