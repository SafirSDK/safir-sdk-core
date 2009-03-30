PROJECT (dots_configuration_check)

INCLUDE($ENV{SAFIR_SDK}/data/build/cmake_settings.txt)

FILE(GLOB sources *.cpp)

SET(CMAKE_CXX_FLAGS "-Wall -Wunused-parameter")

ADD_EXECUTABLE(dots_configuration_check ${sources})

TARGET_LINK_LIBRARIES(dots_configuration_check dots_kernel)

INSTALL(TARGETS dots_configuration_check
        RUNTIME DESTINATION $ENV{SAFIR_RUNTIME}/bin
        LIBRARY DESTINATION $ENV{SAFIR_RUNTIME}/lib
        ARCHIVE DESTINATION $ENV{SAFIR_SDK}/lib)
