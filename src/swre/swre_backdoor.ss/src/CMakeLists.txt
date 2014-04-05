ADD_EXECUTABLE(backdoor main.cpp)

FIND_PACKAGE(Boost COMPONENTS program_options system)

TARGET_LINK_LIBRARIES(backdoor douf_time_cpp dose_cpp dots_generated-swre_library-cpp ${Boost_LIBRARIES})

INSTALL(TARGETS backdoor
        RUNTIME DESTINATION ${SAFIR_RUNTIME}/bin
        LIBRARY DESTINATION ${SAFIR_RUNTIME}/lib
        ARCHIVE DESTINATION ${SAFIR_SDK}/lib)

INSTALL_DEBUG_INFO(backdoor)
