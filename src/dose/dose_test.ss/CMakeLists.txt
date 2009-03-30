cmake_minimum_required(VERSION 2.4)

ADD_SUBDIRECTORY(cpp)
ADD_SUBDIRECTORY(dotnet)
#ADD_SUBDIRECTORY(java)
#ADD_SUBDIRECTORY(ada)
ADD_SUBDIRECTORY(Sequencer)
ADD_SUBDIRECTORY(StressTests)
ADD_SUBDIRECTORY(util)


INSTALL(DIRECTORY testcases
        DESTINATION $ENV{SAFIR_RUNTIME}/data/text/dose_test
        PATTERN ".svn" EXCLUDE)


INSTALL(DIRECTORY output_standalone
        DESTINATION $ENV{SAFIR_RUNTIME}/data/text/dose_test
        PATTERN ".svn" EXCLUDE)

#this install step is done differently since we want another name for the directory in the installed data.
FILE(GLOB multinode_output_files output_multinode_quick_verify/*.txt)

INSTALL(FILES ${multinode_output_files}
        DESTINATION $ENV{SAFIR_RUNTIME}/data/text/dose_test/output_multinode/)
