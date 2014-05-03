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


SAFIR_INSTALL(TARGETS dots_internal INCLUDE_DIR Safir)

#TODO
# INSTALL(DIRECTORY ../scripts/utils DESTINATION ${SAFIR_SDK}/data)
