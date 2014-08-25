#This tool is only useful on windows.
if (NOT WIN32)
  return()
endif()

ADD_CSHARP_ASSEMBLY(gactool EXE
  SIGN ${safir_sdk_core_SOURCE_DIR}/build/config/sdk/data/build/safirkey.snk
  SOURCES gactool.cs AssemblyInfo.cs)

INSTALL_CSHARP_ASSEMBLY(TARGET gactool DESTINATION ${SAFIR_INSTALL_DESTINATION_INST_UTIL})

#ADD_TEST(NAME pathed_noargs COMMAND $<TARGET_PROPERTY:pathed,ASSEMBLY_FILE>)
#SET_TESTS_PROPERTIES(pathed_noargs PROPERTIES PASS_REGULAR_EXPRESSION "Freeware.*00.*01")

#ADD_TEST(NAME pathed_help COMMAND $<TARGET_PROPERTY:pathed,ASSEMBLY_FILE> /help)
#SET_TESTS_PROPERTIES(pathed_help PROPERTIES PASS_REGULAR_EXPRESSION "Freeware.*MACHINE.*USER.*APPEND.*REMOVE")
