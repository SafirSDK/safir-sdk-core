PROJECT (dots_test_cpp)

INCLUDE($ENV{SAFIR_SDK}/data/build/cmake_settings.txt)

FILE(GLOB sources *.cpp)

SET(CMAKE_CXX_FLAGS "-Wall")

ADD_EXECUTABLE(dots_test_cpp ${sources})
TARGET_LINK_LIBRARIES(dots_test_cpp dots_cpp dots_generated-cpp)

INSTALL(TARGETS dots_test_cpp
        RUNTIME DESTINATION $ENV{SAFIR_RUNTIME}/bin
        LIBRARY DESTINATION $ENV{SAFIR_RUNTIME}/lib
        ARCHIVE DESTINATION $ENV{SAFIR_SDK}/lib)
