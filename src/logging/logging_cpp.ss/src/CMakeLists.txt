FILE(GLOB_RECURSE headers *.h)
FILE(GLOB sources *.cpp)

ADD_LIBRARY(logging_cpp SHARED ${sources} ${headers})

target_include_directories(logging_cpp PUBLIC .)

TARGET_LINK_LIBRARIES(logging_cpp PRIVATE logging_library lluf_utils)

# INSTALL(DIRECTORY Safir DESTINATION ${SAFIR_SDK}/include
#         PATTERN ".svn" EXCLUDE
#         PATTERN "*~" EXCLUDE)

# INSTALL(TARGETS logging_cpp
#         RUNTIME DESTINATION ${SAFIR_RUNTIME}/bin
#         LIBRARY DESTINATION ${SAFIR_RUNTIME}/lib
#         ARCHIVE DESTINATION ${SAFIR_SDK}/lib)

# INSTALL_DEBUG_INFO(logging_cpp)
