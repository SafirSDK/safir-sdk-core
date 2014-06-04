ADD_SUBDIRECTORY(util)
ADD_SUBDIRECTORY(cpp)
ADD_SUBDIRECTORY(dotnet)


ADD_SUBDIRECTORY(java)

#TODO if (SAFIR_ADA_SUPPORT)
#TODO  ADD_SUBDIRECTORY(ada)      
#TODO endif ()



ADD_SUBDIRECTORY(Sequencer)
ADD_SUBDIRECTORY(StressTests)

#TODO

# INSTALL(PROGRAMS run_dose_tests.py
#         DESTINATION ${SAFIR_RUNTIME}/bin)

# INSTALL(DIRECTORY testcases
#         DESTINATION ${SAFIR_RUNTIME}/data/text/dose_test
#         PATTERN ".svn" EXCLUDE)

# INSTALL(DIRECTORY output_standalone
#         DESTINATION ${SAFIR_RUNTIME}/data/text/dose_test
#         PATTERN ".svn" EXCLUDE)

# INSTALL(DIRECTORY output_multinode
#         DESTINATION ${SAFIR_RUNTIME}/data/text/dose_test
#         PATTERN ".svn" EXCLUDE)

# INSTALL(DIRECTORY test_parameters
#         DESTINATION ${SAFIR_RUNTIME}/data/text/dose_test
#         PATTERN ".svn" EXCLUDE)

# INSTALL(DIRECTORY test_config
#         DESTINATION ${SAFIR_RUNTIME}/data/text/dose_test
#         PATTERN ".svn" EXCLUDE)
