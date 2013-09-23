cmake_minimum_required(VERSION 2.8)

project(dots_depends CXX)

INCLUDE($ENV{SAFIR_SDK}/data/build/safir_sdk_core_config.cmake)

add_subdirectory(src)

INSTALL(FILES 
        data/dll_imports.h
        data/cmake_depend.txt
        data/dll_imports.cpp_template
        DESTINATION ${SAFIR_SDK}/dots/dots_generated)
