cmake_minimum_required(VERSION 2.6)
PROJECT (EntityReceptionVerifier)

INCLUDE($ENV{SAFIR_SDK}/data/build/cmake_settings.txt)

FILE(GLOB sources *.cpp ../common/*.cpp)

ADD_EXECUTABLE(EntityReceptionVerifier ${sources})

TARGET_LINK_LIBRARIES(EntityReceptionVerifier ${COMMON_LIBRARIES} ${ACE_LIBRARY})

if (NOT MSVC)
    FIND_PACKAGE(Boost COMPONENTS program_options)
    TARGET_LINK_LIBRARIES(EntityReceptionVerifier dose_cpp ${Boost_LIBRARIES})
endif()

INSTALL(TARGETS EntityReceptionVerifier
        RUNTIME DESTINATION ${SAFIR_RUNTIME}/bin
        LIBRARY DESTINATION ${SAFIR_RUNTIME}/lib
        ARCHIVE DESTINATION ${SAFIR_SDK}/lib)


INSTALL_DEBUG_INFO(EntityReceptionVerifier)