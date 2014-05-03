FILE(GLOB_RECURSE headers *.h)
FILE(GLOB sources *.cpp)

ADD_LIBRARY(lluf_config SHARED ${sources} ${headers})

TARGET_INCLUDE_DIRECTORIES(lluf_config PUBLIC .)


# INSTALL(DIRECTORY Safir DESTINATION ${SAFIR_SDK}/include
#         PATTERN ".svn" EXCLUDE
#         PATTERN "*~" EXCLUDE)

# INSTALL(TARGETS lluf_config
#         RUNTIME DESTINATION ${SAFIR_RUNTIME}/bin
#         LIBRARY DESTINATION ${SAFIR_RUNTIME}/lib
#         ARCHIVE DESTINATION ${SAFIR_SDK}/lib)

# INSTALL_DEBUG_INFO(lluf_config)
