ADD_SAFIR_GENERATED_LIBRARY(NAME DopeTest DEPENDENCIES Core
  DOU_FILES
   data/DopeTest.BigEntity.dou
   data/DopeTest.SmallEntity.dou

  DOM_FILES
   data/DopeTest.BigEntity-Safir.Dob.InjectionProperty.dom
   data/DopeTest.SmallEntity-Safir.Dob.InjectionProperty.dom
  )

INSTALL_SAFIR_GENERATED_LIBRARY(
  TARGETS DopeTest
  TEST_SUITE)
