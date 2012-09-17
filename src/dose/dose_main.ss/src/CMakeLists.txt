cmake_minimum_required(VERSION 2.6)

PROJECT (dose_main)

INCLUDE_DIRECTORIES(.)

INCLUDE($ENV{SAFIR_SDK}/data/build/cmake_settings.txt)

FILE(GLOB headers *.h)
FILE(GLOB sources *.cpp)

ADD_EXECUTABLE(dose_main ${sources} ${headers})

FIND_PACKAGE(Boost COMPONENTS thread regex system)
TARGET_LINK_LIBRARIES(dose_main ${ACE_LIBRARY} ${Boost_LIBRARIES})

if (MSVC)
    TARGET_LINK_LIBRARIES(dose_main 
                          debug dose_com${CMAKE_DEBUG_POSTFIX} optimized dose_com)
else() #NOT MSVC
  TARGET_LINK_LIBRARIES(dose_main dose_cpp dose_internal dots_generated-Safir-cpp dots_generated-cpp dots_cpp lluf_utils dose_com lluf_crash_reporter)
endif()

INSTALL(TARGETS dose_main
        RUNTIME DESTINATION ${SAFIR_RUNTIME}/bin
        LIBRARY DESTINATION ${SAFIR_RUNTIME}/lib
        ARCHIVE DESTINATION ${SAFIR_SDK}/lib)

INSTALL_DEBUG_INFO(dose_main)
