cmake_minimum_required(VERSION 2.8)
PROJECT (DobMess)

INCLUDE($ENV{SAFIR_SDK}/data/build/safir_sdk_core_config.cmake)

FILE(GLOB headers *.h)
FILE(GLOB sources *.cpp)

ADD_EXECUTABLE(DobMess ${sources} ${headers})

if (NOT MSVC)
    TARGET_LINK_LIBRARIES(DobMess PRIVATE dose_cpp ${Boost_LIBRARIES})
endif()

# INSTALL(TARGETS DobMess
#         RUNTIME DESTINATION ${SAFIR_RUNTIME}/bin
#         LIBRARY DESTINATION ${SAFIR_RUNTIME}/lib
#         ARCHIVE DESTINATION ${SAFIR_SDK}/lib)

# INSTALL_DEBUG_INFO(DobMess)
