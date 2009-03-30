PROJECT (dose_test_cpp)

INCLUDE($ENV{SAFIR_SDK}/data/build/cmake_settings.txt)

FILE(GLOB sources *.cpp)

ADD_EXECUTABLE(dose_test_cpp ${sources})

TARGET_LINK_LIBRARIES(dose_test_cpp dots_generated-cpp dose_cpp)

INSTALL(TARGETS dose_test_cpp
        RUNTIME DESTINATION $ENV{SAFIR_RUNTIME}/bin
        LIBRARY DESTINATION $ENV{SAFIR_RUNTIME}/lib
        ARCHIVE DESTINATION $ENV{SAFIR_SDK}/lib)
