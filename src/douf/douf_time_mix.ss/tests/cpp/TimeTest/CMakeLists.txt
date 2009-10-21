PROJECT (douf_time_test)

INCLUDE($ENV{SAFIR_SDK}/data/build/cmake_settings.txt)

FILE(GLOB sources douf_time_test.cpp Consumer.cpp DobHandler.cpp)

ADD_EXECUTABLE(douf_time_test ${sources})

TARGET_LINK_LIBRARIES(douf_time_test 
                      dots_cpp 
					  dots_generated-cpp
                      douf_time_library 
                      douf_time_cpp
                      boost_date_time 
                      dose_cpp)

INSTALL(TARGETS douf_time_test
        RUNTIME DESTINATION $ENV{SAFIR_RUNTIME}/bin
        LIBRARY DESTINATION $ENV{SAFIR_RUNTIME}/lib
        ARCHIVE DESTINATION $ENV{SAFIR_SDK}/lib)

