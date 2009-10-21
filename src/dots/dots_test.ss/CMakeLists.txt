cmake_minimum_required(VERSION 2.6)

INCLUDE($ENV{SAFIR_SDK}/data/build/cmake_settings.txt)

ADD_SUBDIRECTORY(cpp)
ADD_SUBDIRECTORY(dotnet)
ADD_SUBDIRECTORY(ada)
ADD_SUBDIRECTORY(java)

INSTALL(FILES output.txt
        DESTINATION ${SAFIR_RUNTIME}/data/text/dots_test)
