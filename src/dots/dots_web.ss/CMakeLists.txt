cmake_minimum_required(VERSION 2.6)

PROJECT (dots_web)

add_subdirectory(src)

INCLUDE($ENV{SAFIR_SDK}/data/build/cmake_settings.txt)

INSTALL(DIRECTORY web
        DESTINATION ${SAFIR_RUNTIME}/data/text
        PATTERN ".svn" EXCLUDE)
