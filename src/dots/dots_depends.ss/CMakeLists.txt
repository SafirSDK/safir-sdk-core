cmake_minimum_required(VERSION 2.6)

PROJECT (dots_depends)

add_subdirectory(src)

INCLUDE($ENV{SAFIR_SDK}/data/build/cmake_settings.txt)

INSTALL(FILES 
        data/dll_imports.h
        data/cmake_depend.txt
        data/dll_imports.cpp_template
        DESTINATION ${SAFIR_SDK}/dots/dots_generated)
