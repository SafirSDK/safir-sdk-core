cmake_minimum_required(VERSION 2.8)

project(header_checks NONE)

enable_testing()

if (NOT CMAKE_BUILD_TYPE)
   set(CMAKE_BUILD_TYPE "RelWithDebInfo")
endif()

execute_process(COMMAND ${CMAKE_COMMAND} -G ${CMAKE_GENERATOR} -DCMAKE_BUILD_TYPE:string=${CMAKE_BUILD_TYPE} .
  WORKING_DIRECTORY no_naughty_includes)

add_test(NAME no_naughty_includes 
  COMMAND ${CMAKE_COMMAND} --build no_naughty_includes)

