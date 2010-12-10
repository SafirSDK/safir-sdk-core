cmake_minimum_required(VERSION 2.6)

PROJECT (dots_depends_test)

add_subdirectory(generate_max_dots)

INCLUDE($ENV{SAFIR_SDK}/data/build/cmake_settings.txt)

INSTALL(FILES 
        data/DotsMax.Entity.dou
        DESTINATION ${SAFIR_SDK}/dots/dots_generated)
