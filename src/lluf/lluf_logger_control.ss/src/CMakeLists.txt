PROJECT (logger_control)

INCLUDE($ENV{SAFIR_SDK}/data/build/cmake_settings.txt)

FILE(GLOB sources *.cpp)

ADD_EXECUTABLE(logger_control ${sources})
TARGET_LINK_LIBRARIES(logger_control rt)

INSTALL(TARGETS logger_control DESTINATION $ENV{SAFIR_RUNTIME}/bin)
