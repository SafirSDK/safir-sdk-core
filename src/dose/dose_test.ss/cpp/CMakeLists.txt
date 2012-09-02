cmake_minimum_required(VERSION 2.6)

project(dose_test_cpp CXX)

INCLUDE($ENV{SAFIR_SDK}/data/build/cmake_settings.txt)

FILE(GLOB headers *.h)
FILE(GLOB sources *.cpp)
FILE(GLOB dose_com_sources ${SAFIR_SDK}/include/Safir/Dob/Internal/DoseComAux/*.cpp)

ADD_EXECUTABLE(dose_test_cpp ${sources} ${dose_com_sources} ${headers})

FIND_PACKAGE(Boost COMPONENTS thread system)
TARGET_LINK_LIBRARIES(dose_test_cpp ${COMMON_LIBRARIES})

if (NOT MSVC)
    TARGET_LINK_LIBRARIES(dose_test_cpp swre_interface_cpp dose_cpp dots_generated-Safir-cpp dots_generated-DoseTest-cpp dots_cpp ${Boost_LIBRARIES})
endif()

INSTALL(TARGETS dose_test_cpp
        RUNTIME DESTINATION ${SAFIR_RUNTIME}/bin
        LIBRARY DESTINATION ${SAFIR_RUNTIME}/lib
        ARCHIVE DESTINATION ${SAFIR_SDK}/lib)

INSTALL_DEBUG_INFO(dose_test_cpp)
