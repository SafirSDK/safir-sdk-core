project(dose_com_error_log CXX)

INCLUDE($ENV{SAFIR_SDK}/data/build/cmake_settings.txt)

SET(CMAKE_CXX_FLAGS "-D_LINUX")

FILE(GLOB sources *.cpp)

ADD_EXECUTABLE(dose_com_error_log ${sources})

INSTALL(TARGETS dose_com_error_log
        RUNTIME DESTINATION $ENV{SAFIR_RUNTIME}/bin
        LIBRARY DESTINATION $ENV{SAFIR_RUNTIME}/lib
        ARCHIVE DESTINATION $ENV{SAFIR_SDK}/lib)
