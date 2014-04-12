FILE(GLOB sources *.cpp ../common/*.cpp)
FILE(GLOB headers *.h ../common/*.h)

ADD_EXECUTABLE(RequestStresser ${sources} ${headers})

if (NOT MSVC)
    TARGET_LINK_LIBRARIES(RequestStresser PRIVATE dose_cpp dots_generated-DoseStressTest-cpp dots_generated-Safir-cpp dots_cpp lluf_utils ${Boost_LIBRARIES})
endif()

INSTALL(TARGETS RequestStresser
        RUNTIME DESTINATION ${SAFIR_RUNTIME}/bin
        LIBRARY DESTINATION ${SAFIR_RUNTIME}/lib
        ARCHIVE DESTINATION ${SAFIR_SDK}/lib)


INSTALL_DEBUG_INFO(RequestStresser)
