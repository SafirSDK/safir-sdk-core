ADD_SUBDIRECTORY(cpp)
ADD_SUBDIRECTORY(dotnet)
ADD_SUBDIRECTORY(java)

get_property(SAFIR_GENERATED_PATHS GLOBAL PROPERTY SAFIR_GENERATED_PATHS)

ADD_TEST(NAME dots_cpp 
  COMMAND ${PYTHON_EXECUTABLE} ${CMAKE_CURRENT_SOURCE_DIR}/run_test.py
  --show-safir-config $<TARGET_FILE:safir_show_config>
  --language cpp
  --binary $<TARGET_FILE:dots_test_cpp>
  --output ${CMAKE_CURRENT_SOURCE_DIR}/output.txt
  --safir-generated-paths "${SAFIR_GENERATED_PATHS}")


SET_SAFIR_TEST_PROPERTIES(TEST dots_cpp
  CONFIG_OVERRIDE ${CMAKE_CURRENT_SOURCE_DIR}/test_config)


if (CSHARP_FOUND)
  ADD_TEST(NAME dots_dotnet
    COMMAND ${PYTHON_EXECUTABLE} ${CMAKE_CURRENT_SOURCE_DIR}/run_test.py 
    --show-safir-config $<TARGET_FILE:safir_show_config>
    --language dotnet
    --binary $<TARGET_PROPERTY:dots_test_dotnet,ASSEMBLY_FILE>
    --dependencies $<TARGET_PROPERTY:Safir.Logging,ASSEMBLY_FILE>,$<TARGET_PROPERTY:Safir.Dob.Typesystem,ASSEMBLY_FILE>,$<TARGET_PROPERTY:safir_generated-DotsTest-dotnet,ASSEMBLY_FILE>
    --output ${CMAKE_CURRENT_SOURCE_DIR}/output.txt
    --safir-generated-paths "${SAFIR_GENERATED_PATHS}"
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/dotnet)

  SET_SAFIR_TEST_PROPERTIES(TEST dots_dotnet
    CONFIG_OVERRIDE ${CMAKE_CURRENT_SOURCE_DIR}/test_config)
endif()

if (Java_FOUND)
  ADD_TEST(NAME dots_java
    COMMAND ${PYTHON_EXECUTABLE} ${CMAKE_CURRENT_SOURCE_DIR}/run_test.py
    --show-safir-config $<TARGET_FILE:safir_show_config>
    --language java
    --jar $<TARGET_PROPERTY:dots_test_java,JAR_FILE>
    --dependencies $<TARGET_PROPERTY:safir_generated-DotsTest-java,JAR_FILE>,$<TARGET_PROPERTY:dots_java,JAR_FILE>
    --output ${CMAKE_CURRENT_SOURCE_DIR}/output.txt
    --safir-generated-paths "${SAFIR_GENERATED_PATHS}"
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/java)

  SET_SAFIR_TEST_PROPERTIES(TEST dots_java
    CONFIG_OVERRIDE ${CMAKE_CURRENT_SOURCE_DIR}/test_config)

endif()
