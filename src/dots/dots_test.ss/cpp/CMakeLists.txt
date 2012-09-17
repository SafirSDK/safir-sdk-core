cmake_minimum_required(VERSION 2.6)

PROJECT (dots_test_cpp)

INCLUDE($ENV{SAFIR_SDK}/data/build/cmake_settings.txt)

FILE(GLOB sources *.cpp)

ADD_EXECUTABLE(dots_test_cpp ${sources})

if (NOT MSVC)
    TARGET_LINK_LIBRARIES(dots_test_cpp dots_cpp dots_generated-DotsTest-cpp dots_generated-cpp)
endif()

INSTALL(TARGETS dots_test_cpp
        RUNTIME DESTINATION ${SAFIR_RUNTIME}/bin
        LIBRARY DESTINATION ${SAFIR_RUNTIME}/lib
        ARCHIVE DESTINATION ${SAFIR_SDK}/lib)


INSTALL_DEBUG_INFO(dots_test_cpp)
