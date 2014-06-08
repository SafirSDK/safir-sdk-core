FILE(GLOB headers *.h ../defs/*.h)
FILE(GLOB sources *.cpp)

ADD_LIBRARY(dose_com SHARED ${sources} ${headers})

if (WIN32)
  TARGET_LINK_LIBRARIES(dose_com PRIVATE ws2_32)
endif()

TARGET_LINK_LIBRARIES(dose_com PRIVATE lluf_utils)

target_include_directories(dose_com PUBLIC $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}/../defs>)
target_include_directories(dose_com PUBLIC $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}>)
#This is rather nasty (making all header files public) but it is the way dose_com is made...

SAFIR_INSTALL(TARGETS dose_com)
