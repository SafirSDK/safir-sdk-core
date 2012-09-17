cmake_minimum_required(VERSION 2.6)
PROJECT (MessageLatencies)

INCLUDE($ENV{SAFIR_SDK}/data/build/cmake_settings.txt)

FILE(GLOB sources *.cpp ../common/*.cpp)

ADD_EXECUTABLE(MessageLatencies ${sources})

TARGET_LINK_LIBRARIES(MessageLatencies ${COMMON_LIBRARIES} ${ACE_LIBRARY})

if (NOT MSVC)
    FIND_PACKAGE(Boost COMPONENTS program_options)
    TARGET_LINK_LIBRARIES(MessageLatencies dots_generated-DoseStressTest-cpp dots_generated-Safir-cpp dots_generated-cpp dots_cpp lluf_utils dose_cpp ${Boost_LIBRARIES})
endif()

INSTALL(TARGETS MessageLatencies
        RUNTIME DESTINATION ${SAFIR_RUNTIME}/bin
        LIBRARY DESTINATION ${SAFIR_RUNTIME}/lib
        ARCHIVE DESTINATION ${SAFIR_SDK}/lib)

INSTALL_DEBUG_INFO(MessageLatencies)
