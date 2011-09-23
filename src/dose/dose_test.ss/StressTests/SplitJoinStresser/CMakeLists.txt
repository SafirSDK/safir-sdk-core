cmake_minimum_required(VERSION 2.6)
PROJECT (SplitJoinStresser)

INCLUDE($ENV{SAFIR_SDK}/data/build/cmake_settings.txt)

FILE(GLOB headers *.h)
FILE(GLOB sources *.cpp)

ADD_EXECUTABLE(SplitJoinStresser ${sources} ${headers})

TARGET_LINK_LIBRARIES(SplitJoinStresser ${COMMON_LIBRARIES} ${ACE_LIBRARY})

if (NOT MSVC)
    FIND_PACKAGE(Boost COMPONENTS program_options)
    TARGET_LINK_LIBRARIES(SplitJoinStresser dose_cpp ${Boost_LIBRARIES})
endif()

INSTALL(TARGETS SplitJoinStresser
        RUNTIME DESTINATION ${SAFIR_RUNTIME}/bin
        LIBRARY DESTINATION ${SAFIR_RUNTIME}/lib
        ARCHIVE DESTINATION ${SAFIR_SDK}/lib)

INSTALL_DEBUG_INFO(SplitJoinStresser)