cmake_minimum_required(VERSION 2.4)

PROJECT (dose_test_cpp)

INCLUDE($ENV{SAFIR_SDK}/data/build/cmake_settings.txt)

FILE(GLOB headers *.h)
FILE(GLOB sources *.cpp)

ADD_EXECUTABLE(dose_test_cpp ${sources} ${headers})

TARGET_LINK_LIBRARIES(dose_test_cpp ${COMMON_LIBRARIES} ${ACE_LIBRARY})

if (NOT MSVC)
    TARGET_LINK_LIBRARIES(dose_test_cpp dots_generated-cpp dose_cpp)
endif()

INSTALL(TARGETS dose_test_cpp
        RUNTIME DESTINATION ${SAFIR_RUNTIME}/bin
        LIBRARY DESTINATION ${SAFIR_RUNTIME}/lib
        ARCHIVE DESTINATION ${SAFIR_SDK}/lib)

INSTALL_DEBUG_INFO(dose_test_cpp)