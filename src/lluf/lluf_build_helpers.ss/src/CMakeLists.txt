cmake_minimum_required(VERSION 2.8)

PROJECT (lluf_build_helpers NONE)

INCLUDE($ENV{SAFIR_SDK}/data/build/safir_sdk_core_config.cmake)

INSTALL(DIRECTORY Safir DESTINATION ${SAFIR_SDK}/include
        PATTERN ".svn" EXCLUDE
        PATTERN "*~" EXCLUDE)

