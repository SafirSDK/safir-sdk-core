cmake_minimum_required(VERSION 2.6)
PROJECT (MessageStresser)

INCLUDE($ENV{SAFIR_SDK}/data/build/cmake_settings.txt)

FILE(GLOB sources *.cpp ../common/*.cpp)
FILE(GLOB headers *.h ../common/*.h)

ADD_EXECUTABLE(MessageStresser ${sources} ${headers})

TARGET_LINK_LIBRARIES(MessageStresser ${COMMON_LIBRARIES} ${ACE_LIBRARY})

if (NOT MSVC)
    FIND_PACKAGE(Boost COMPONENTS program_options)
    TARGET_LINK_LIBRARIES(MessageStresser dots_generated-DoseStressTest-cpp dots_generated-Safir-cpp dots_generated-cpp dots_cpp lluf_utils dose_cpp ${Boost_LIBRARIES})
endif()

INSTALL(TARGETS MessageStresser
        RUNTIME DESTINATION ${SAFIR_RUNTIME}/bin
        LIBRARY DESTINATION ${SAFIR_RUNTIME}/lib
        ARCHIVE DESTINATION ${SAFIR_SDK}/lib)


INSTALL_DEBUG_INFO(MessageStresser)
