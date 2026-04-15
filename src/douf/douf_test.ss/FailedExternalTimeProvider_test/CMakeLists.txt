ADD_EXECUTABLE(FailedExternalTimeProvider_test test.cpp)

TARGET_LINK_LIBRARIES(FailedExternalTimeProvider_test PRIVATE
  douf_time_cpp
  dots_cpp)

ADD_TEST(NAME FailedExternalTimeProvider_test COMMAND FailedExternalTimeProvider_test)

SET_SAFIR_TEST_PROPERTIES(TEST FailedExternalTimeProvider_test
                          CONFIG_OVERRIDE "${CMAKE_CURRENT_SOURCE_DIR}/config")

if (CSHARP_FOUND)
  ADD_CSHARP_ASSEMBLY(failed_external_time_provider_test_dotnet EXE
    SOURCES test.cs
    REFERENCES Safir.Time Safir.Dob.Typesystem)

  ADD_TEST(NAME FailedExternalTimeProvider_dotnet_test
    COMMAND ${Python3_EXECUTABLE} ${CMAKE_CURRENT_SOURCE_DIR}/run_test_dotnet.py
    --test-exe $<TARGET_GENEX_EVAL:failed_external_time_provider_test_dotnet,$<TARGET_PROPERTY:failed_external_time_provider_test_dotnet,ASSEMBLY_FILE>>)

  SET_SAFIR_TEST_PROPERTIES(TEST FailedExternalTimeProvider_dotnet_test
                            CONFIG_OVERRIDE "${CMAKE_CURRENT_SOURCE_DIR}/config")
endif()

if (Java_FOUND)
  ADD_JAR(failed_external_time_provider_test_java
    SOURCES src/Test.java
    INCLUDE_JARS douf_java
    ENTRY_POINT Test
    MANIFEST ${CMAKE_CURRENT_SOURCE_DIR}/Manifest.txt)

  ADD_TEST(NAME FailedExternalTimeProvider_java_test
    COMMAND ${Python3_EXECUTABLE} ${CMAKE_CURRENT_SOURCE_DIR}/run_test_java.py
    --jar $<TARGET_PROPERTY:failed_external_time_provider_test_java,JAR_FILE>)

  SET_SAFIR_TEST_PROPERTIES(TEST FailedExternalTimeProvider_java_test
                            CONFIG_OVERRIDE "${CMAKE_CURRENT_SOURCE_DIR}/config")
endif()
