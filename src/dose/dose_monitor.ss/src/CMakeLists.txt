cmake_minimum_required(VERSION 2.6)

PROJECT (dose_monitor)

INCLUDE($ENV{SAFIR_SDK}/data/build/cmake_settings.txt)

FILE(GLOB headers *.h)
FILE(GLOB sources *.cpp)

ADD_EXECUTABLE(dose_monitor ${sources} ${headers})

TARGET_LINK_LIBRARIES(dose_monitor ${COMMON_LIBRARIES} ${ACE_LIBRARY})

if (NOT MSVC)
    FIND_PACKAGE(Boost COMPONENTS program_options)
    TARGET_LINK_LIBRARIES(dose_monitor dots_generated-cpp dose_cpp ${Boost_LIBRARIES})
endif()

INSTALL(TARGETS dose_monitor
        RUNTIME DESTINATION $ENV{SAFIR_RUNTIME}/bin
        LIBRARY DESTINATION $ENV{SAFIR_RUNTIME}/lib
        ARCHIVE DESTINATION $ENV{SAFIR_SDK}/lib)

INSTALL_DEBUG_INFO(dose_monitor)
