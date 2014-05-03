FILE(GLOB_RECURSE headers *.h)
FILE(GLOB sources *.cpp)

ADD_LIBRARY(dots_internal SHARED ${sources} ${headers})

target_include_directories(dots_internal PUBLIC .)

TARGET_LINK_LIBRARIES(dots_internal PRIVATE
  lluf_config
  lluf_utils 
  lluf_id 
  ${Boost_FILESYSTEM_LIBRARY}
  ${Boost_THREAD_LIBRARY}
  ${Boost_SYSTEM_LIBRARY})


# INSTALL(DIRECTORY Safir DESTINATION ${SAFIR_SDK}/include
#         PATTERN ".svn" EXCLUDE
#         PATTERN "*~" EXCLUDE)

# INSTALL(DIRECTORY ../scripts/utils DESTINATION ${SAFIR_SDK}/data)

# INSTALL(TARGETS dots_internal
#         RUNTIME DESTINATION ${SAFIR_RUNTIME}/bin
#         LIBRARY DESTINATION ${SAFIR_RUNTIME}/lib
#         ARCHIVE DESTINATION ${SAFIR_SDK}/lib)

# INSTALL_DEBUG_INFO(dots_internal)
