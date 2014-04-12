FILE(GLOB headers *.h)
FILE(GLOB sources *.cpp)

ADD_EXECUTABLE(RequestPingPong ${sources} ${headers})


if (NOT MSVC)
    TARGET_LINK_LIBRARIES(RequestPingPong PRIVATE dose_cpp dots_generated-DoseTest-cpp dots_generated-Safir-cpp dots_cpp lluf_utils ${Boost_LIBRARIES})
endif()

INSTALL(TARGETS RequestPingPong
        RUNTIME DESTINATION ${SAFIR_RUNTIME}/bin
        LIBRARY DESTINATION ${SAFIR_RUNTIME}/lib
        ARCHIVE DESTINATION ${SAFIR_SDK}/lib)

INSTALL_DEBUG_INFO(RequestPingPong)
