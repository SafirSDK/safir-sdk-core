ADD_SAFIR_GENERATED_LIBRARY(NAME DoseTest DEPENDENCIES Core GLOB)

#These files exceed the size of gccs variable tracking, so we disable it.
if (CMAKE_COMPILER_IS_GNUCXX)
  SET_SOURCE_FILES_PROPERTIES(
    gen/cpp/DoseTest.ComplexGlobalEntity.cpp
    gen/cpp/DoseTest.ComplexGlobalService.cpp
    gen/cpp/DoseTest.ComplexGlobalMessage.cpp

    PROPERTIES COMPILE_FLAGS "-fno-var-tracking-assignments")
endif()

INSTALL_SAFIR_GENERATED_LIBRARY(TARGETS DoseTest TEST_SUITE)
