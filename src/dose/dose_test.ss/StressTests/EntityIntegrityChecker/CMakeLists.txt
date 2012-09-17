cmake_minimum_required(VERSION 2.6)

PROJECT (EntityIntegrityChecker)

INCLUDE($ENV{SAFIR_SDK}/data/build/cmake_settings.txt)

FILE(GLOB sources *.cpp ../common/*.cpp)

ADD_EXECUTABLE(EntityIntegrityChecker ${sources})

TARGET_LINK_LIBRARIES(EntityIntegrityChecker ${COMMON_LIBRARIES} ${ACE_LIBRARY})

if (NOT MSVC)
    FIND_PACKAGE(Boost COMPONENTS program_options)
    TARGET_LINK_LIBRARIES(EntityIntegrityChecker dose_cpp dots_generated-DoseIntegrityCheck-cpp dots_generated-Safir-cpp dots_cpp lluf_utils ${Boost_LIBRARIES})
endif()

INSTALL(TARGETS EntityIntegrityChecker
        RUNTIME DESTINATION ${SAFIR_RUNTIME}/bin
        LIBRARY DESTINATION ${SAFIR_RUNTIME}/lib
        ARCHIVE DESTINATION ${SAFIR_SDK}/lib)

INSTALL_DEBUG_INFO(EntityIntegrityChecker)
