cmake_minimum_required(VERSION 2.6)
PROJECT (RequestStresser)

INCLUDE($ENV{SAFIR_SDK}/data/build/cmake_settings.txt)

FILE(GLOB sources *.cpp ../common/*.cpp)

ADD_EXECUTABLE(RequestStresser ${sources})

TARGET_LINK_LIBRARIES(RequestStresser ${COMMON_LIBRARIES} ${ACE_LIBRARY})

if (NOT MSVC)
    FIND_PACKAGE(Boost COMPONENTS program_options)
    TARGET_LINK_LIBRARIES(RequestStresser dots_generated-DoseStressTest-cpp dots_generated-Safir-cpp dots_generated-cpp dots_cpp lluf_utils dose_cpp ${Boost_LIBRARIES})
endif()

INSTALL(TARGETS RequestStresser
        RUNTIME DESTINATION ${SAFIR_RUNTIME}/bin
        LIBRARY DESTINATION ${SAFIR_RUNTIME}/lib
        ARCHIVE DESTINATION ${SAFIR_SDK}/lib)


INSTALL_DEBUG_INFO(RequestStresser)
