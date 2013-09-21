cmake_minimum_required(VERSION 2.8)

project(foreach CXX)

INCLUDE($ENV{SAFIR_SDK}/data/build/cmake_settings.txt)

FILE(GLOB headers *.h)
FILE(GLOB sources *.cpp)

ADD_EXECUTABLE(foreach ${sources} ${headers})

FIND_PACKAGE(Boost COMPONENTS system)
if (NOT MSVC)
    TARGET_LINK_LIBRARIES(foreach swre_interface_cpp dose_cpp dots_generated-Safir-cpp dots_generated-cpp dots_cpp logging_cpp ${Boost_LIBRARIES})
endif()

INSTALL(TARGETS foreach
        RUNTIME DESTINATION ${SAFIR_RUNTIME}/bin
        LIBRARY DESTINATION ${SAFIR_RUNTIME}/lib
        ARCHIVE DESTINATION ${SAFIR_SDK}/lib)

INSTALL_DEBUG_INFO(foreach)
