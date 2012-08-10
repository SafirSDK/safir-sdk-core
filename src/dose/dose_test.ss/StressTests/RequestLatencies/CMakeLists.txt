cmake_minimum_required(VERSION 2.6)
PROJECT (RequestLatencies)

INCLUDE($ENV{SAFIR_SDK}/data/build/cmake_settings.txt)

FILE(GLOB sources *.cpp ../common/*.cpp)

if (MSVC)
   # Suppress some warnings
   SET(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -wd4244")
endif()

ADD_EXECUTABLE(RequestLatencies ${sources})

TARGET_LINK_LIBRARIES(RequestLatencies ${COMMON_LIBRARIES} ${ACE_LIBRARY})

if (NOT MSVC)
    FIND_PACKAGE(Boost COMPONENTS program_options)
    TARGET_LINK_LIBRARIES(RequestLatencies dose_cpp ${Boost_LIBRARIES})
endif()

INSTALL(TARGETS RequestLatencies
        RUNTIME DESTINATION ${SAFIR_RUNTIME}/bin
        LIBRARY DESTINATION ${SAFIR_RUNTIME}/lib
        ARCHIVE DESTINATION ${SAFIR_SDK}/lib)


INSTALL_DEBUG_INFO(RequestLatencies)