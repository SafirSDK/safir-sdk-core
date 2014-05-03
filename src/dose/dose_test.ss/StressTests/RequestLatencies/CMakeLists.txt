FILE(GLOB sources *.cpp ../common/*.cpp)
FILE(GLOB headers *.h ../common/*.h)

ADD_EXECUTABLE(RequestLatencies ${sources} ${headers})


if (NOT MSVC)
    TARGET_LINK_LIBRARIES(RequestLatencies PRIVATE dose_cpp dots_generated-DoseStressTest-cpp dots_generated-Safir-cpp dots_cpp lluf_utils ${Boost_LIBRARIES})
endif()

# INSTALL(TARGETS RequestLatencies
#         RUNTIME DESTINATION ${SAFIR_RUNTIME}/bin
#         LIBRARY DESTINATION ${SAFIR_RUNTIME}/lib
#         ARCHIVE DESTINATION ${SAFIR_SDK}/lib)

# INSTALL_DEBUG_INFO(RequestLatencies)
