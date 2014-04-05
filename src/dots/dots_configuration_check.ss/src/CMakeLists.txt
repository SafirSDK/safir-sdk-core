FILE(GLOB sources *.cpp)

ADD_EXECUTABLE(dots_configuration_check ${sources})

FIND_PACKAGE(Boost COMPONENTS regex filesystem date_time system program_options)

TARGET_LINK_LIBRARIES(dots_configuration_check lluf_id dots_internal dots_kernel ${Boost_LIBRARIES})

INSTALL(TARGETS dots_configuration_check
        RUNTIME DESTINATION ${SAFIR_RUNTIME}/bin
        LIBRARY DESTINATION ${SAFIR_RUNTIME}/lib
        ARCHIVE DESTINATION ${SAFIR_SDK}/lib)

INSTALL_DEBUG_INFO(dots_configuration_check)
