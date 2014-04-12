FILE(GLOB sources *.cpp ../common/*.cpp)
FILE(GLOB headers *.h ../common/*.h)

ADD_EXECUTABLE(MessageLatencies ${sources} ${headers})

if (NOT MSVC)
    TARGET_LINK_LIBRARIES(MessageLatencies PRIVATE dose_cpp dots_generated-DoseStressTest-cpp dots_generated-Safir-cpp dots_cpp lluf_utils ${Boost_LIBRARIES})
endif()

INSTALL(TARGETS MessageLatencies
        RUNTIME DESTINATION ${SAFIR_RUNTIME}/bin
        LIBRARY DESTINATION ${SAFIR_RUNTIME}/lib
        ARCHIVE DESTINATION ${SAFIR_SDK}/lib)

INSTALL_DEBUG_INFO(MessageLatencies)
