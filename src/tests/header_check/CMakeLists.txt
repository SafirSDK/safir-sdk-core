cmake_minimum_required(VERSION 2.8)

project(header_checks NONE)

enable_testing()

execute_process(COMMAND ${CMAKE_COMMAND} -G ${CMAKE_GENERATOR} .
  WORKING_DIRECTORY no_naughty_includes)

add_test(NAME no_naughty_includes 
  COMMAND ${CMAKE_COMMAND} --build no_naughty_includes)

