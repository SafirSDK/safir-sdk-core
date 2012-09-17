cmake_minimum_required(VERSION 2.4)

PROJECT (RequestPingPong)

INCLUDE($ENV{SAFIR_SDK}/data/build/cmake_settings.txt)

FILE(GLOB headers *.h)
FILE(GLOB sources *.cpp)

ADD_EXECUTABLE(RequestPingPong ${sources} ${headers})

FIND_PACKAGE(Boost COMPONENTS system)
TARGET_LINK_LIBRARIES(RequestPingPong ${COMMON_LIBRARIES} ${ACE_LIBRARY})

if (NOT MSVC)
    TARGET_LINK_LIBRARIES(RequestPingPong dots_generated-DoseTest-cpp dots_generated-DoseStressTest-cpp dots_generated-Safir-cpp dots_generated-cpp dots_cpp lluf_utils dose_cpp swre_interface_cpp ${Boost_LIBRARIES})
endif()

INSTALL(TARGETS RequestPingPong
        RUNTIME DESTINATION ${SAFIR_RUNTIME}/bin
        LIBRARY DESTINATION ${SAFIR_RUNTIME}/lib
        ARCHIVE DESTINATION ${SAFIR_SDK}/lib)

INSTALL_DEBUG_INFO(RequestPingPong)
