FILE(GLOB_RECURSE headers *.h)
FILE(GLOB sources *.cpp)

ADD_LIBRARY(dots_internal SHARED ${sources} ${headers})

TARGET_LINK_LIBRARIES(dots_internal PRIVATE
  lluf_utils 
  ${Boost_FILESYSTEM_LIBRARY}
  ${Boost_THREAD_LIBRARY}
  ${Boost_SYSTEM_LIBRARY}
  
  PUBLIC 
  lluf_id 
  lluf_config
)

SAFIR_INSTALL(TARGETS dots_internal)

INSTALL(PROGRAMS ../scripts/utils/xml_convert.py
  DESTINATION ${SAFIR_INSTALL_DESTINATION_DOC}/utilities/)
