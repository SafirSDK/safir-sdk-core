FILE(GLOB headers *.h)
FILE(GLOB sources *.cpp)

if (MSVC)
   # Suppress some warnings
   SET(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -wd4244")
endif()

ADD_EXECUTABLE(RequestPingPong ${sources} ${headers})

TARGET_LINK_LIBRARIES(RequestPingPong ${COMMON_LIBRARIES})

if (NOT MSVC)
    TARGET_LINK_LIBRARIES(RequestPingPong dose_cpp dots_generated-DoseTest-cpp dots_generated-Safir-cpp dots_cpp lluf_utils ${Boost_LIBRARIES})
endif()

INSTALL(TARGETS RequestPingPong
        RUNTIME DESTINATION ${SAFIR_RUNTIME}/bin
        LIBRARY DESTINATION ${SAFIR_RUNTIME}/lib
        ARCHIVE DESTINATION ${SAFIR_SDK}/lib)

INSTALL_DEBUG_INFO(RequestPingPong)
