PROJECT (douf_time_test_simple)

INCLUDE($ENV{SAFIR_SDK}/data/build/cmake_settings.txt)

FILE(GLOB sources douf_time_test2.cpp)

ADD_EXECUTABLE(douf_time_test_simple ${sources})

TARGET_LINK_LIBRARIES(douf_time_test_simple dots_cpp douf_time_library douf_time boost_date_time)

INSTALL(TARGETS douf_time_test_simple
        RUNTIME DESTINATION $ENV{SAFIR_RUNTIME}/bin
        LIBRARY DESTINATION $ENV{SAFIR_RUNTIME}/lib
        ARCHIVE DESTINATION $ENV{SAFIR_SDK}/lib)

