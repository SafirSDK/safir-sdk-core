add_subdirectory(AceTimeConverter_test)
add_subdirectory(ExternalTimeProvider_test)
add_subdirectory(TimeProvider_cpp_test)
#TODO add_subdirectory(TimeProvider_dotnet_test)

if (SAFIR_ADA_SUPPORT)
#TODO  add_subdirectory(TimeProvider_ada_test)
endif()

if (SAFIR_JAVA_SUPPORT)
#TODO  add_subdirectory(TimeProvider_java_test)
endif()
