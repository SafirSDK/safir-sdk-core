PROJECT (dose_memory_stats)

INCLUDE($ENV{SAFIR_SDK}/data/build/cmake_settings.txt)

FILE(GLOB sources *.cpp)

ADD_EXECUTABLE(dose_memory_stats ${sources})

TARGET_LINK_LIBRARIES(dose_memory_stats dose_internal ACE dots_cpp)

INSTALL(TARGETS dose_memory_stats
        RUNTIME DESTINATION $ENV{SAFIR_RUNTIME}/bin
        LIBRARY DESTINATION $ENV{SAFIR_RUNTIME}/lib
        ARCHIVE DESTINATION $ENV{SAFIR_SDK}/lib)
