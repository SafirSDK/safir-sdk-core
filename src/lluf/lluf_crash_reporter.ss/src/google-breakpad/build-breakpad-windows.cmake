cmake_minimum_required(VERSION 2.8)

project (breakpad_client_windows CXX)

FILE(GLOB headers 
  src/client/windows/handler/*.h 
  src/client/windows/crash_generation/crash_generation_client.h 
  src/common/windows/guid_string.h)  
FILE(GLOB sources 
  src/client/windows/handler/*.cc 
  src/client/windows/crash_generation/crash_generation_client.cc 
  src/common/windows/guid_string.cc)

include_directories(src)  
  
add_library(breakpad_client STATIC  ${headers} ${sources})

add_definitions(-DBREAKPAD_NO_TERMINATE_THREAD) 
add_definitions(-DUNICODE -D_UNICODE)
add_definitions(/wd4127 /wd4245)

install(TARGETS breakpad_client
  ARCHIVE DESTINATION ${CMAKE_CURRENT_SOURCE_DIR})