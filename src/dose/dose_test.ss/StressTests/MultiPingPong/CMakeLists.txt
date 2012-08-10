cmake_minimum_required(VERSION 2.6)
PROJECT (MultiPingPong)

INCLUDE($ENV{SAFIR_SDK}/data/build/cmake_settings.txt)

FILE(GLOB sources *.cpp ../common/*.cpp)

if (MSVC)
   # Suppress some warnings
   SET(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -wd4244")
endif()

ADD_EXECUTABLE(MultiPingPong ${sources})

TARGET_LINK_LIBRARIES(MultiPingPong ${COMMON_LIBRARIES} ${ACE_LIBRARY})

if (NOT MSVC)
    FIND_PACKAGE(Boost COMPONENTS program_options)
    TARGET_LINK_LIBRARIES(MultiPingPong dose_cpp ${Boost_LIBRARIES})
endif()

INSTALL(TARGETS MultiPingPong
        RUNTIME DESTINATION ${SAFIR_RUNTIME}/bin
        LIBRARY DESTINATION ${SAFIR_RUNTIME}/lib
        ARCHIVE DESTINATION ${SAFIR_SDK}/lib)

INSTALL_DEBUG_INFO(MultiPingPong)