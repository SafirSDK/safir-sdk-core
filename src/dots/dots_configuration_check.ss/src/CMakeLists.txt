FILE(GLOB sources *.cpp)

ADD_EXECUTABLE(dots_configuration_check ${sources})

TARGET_LINK_LIBRARIES(dots_configuration_check PRIVATE 
  lluf_config
  lluf_id
  dots_internal
  dots_kernel
  ${Boost_FILESYSTEM_LIBRARY}
  ${Boost_PROGRAM_OPTIONS_LIBRARY})

INSTALL(TARGETS dots_configuration_check
        RUNTIME DESTINATION ${SAFIR_RUNTIME}/bin
        LIBRARY DESTINATION ${SAFIR_RUNTIME}/lib
        ARCHIVE DESTINATION ${SAFIR_SDK}/lib)

INSTALL_DEBUG_INFO(dots_configuration_check)
