FILE(GLOB_RECURSE dou_files *.dou)
FILE(GLOB_RECURSE dom_files *.dom)
FILE(GLOB_RECURSE namespace_files *.namespace.txt)

ADD_SAFIR_GENERATED_LIBRARY(NAME DoseTest DEPENDENCIES Core
  DOU_FILES ${dou_files}
  DOM_FILES ${dom_files}
  NAMESPACE_MAPPINGS ${namespace_files}
  )

INSTALL_SAFIR_GENERATED_LIBRARY(TARGETS DoseTest TEST_SUITE)
