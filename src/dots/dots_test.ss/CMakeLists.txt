ADD_SUBDIRECTORY(cpp)
ADD_SUBDIRECTORY(dotnet)
ADD_SUBDIRECTORY(java)

#if (SAFIR_ADA_SUPPORT)
#  ADD_SUBDIRECTORY(ada)
#endif()



get_property(DOTS_GENERATED_PATHS GLOBAL PROPERTY DOTS_GENERATED_PATHS)

ADD_TEST(NAME dots_cpp 
  COMMAND ${PYTHON_EXECUTABLE} ${CMAKE_CURRENT_SOURCE_DIR}/run_test.py
  --show-safir-config $<TARGET_FILE:safir_show_config>
  --language cpp
  --binary $<TARGET_FILE:dots_test_cpp>
  --output ${CMAKE_CURRENT_SOURCE_DIR}/output.txt
  --dots-generated-paths "${DOTS_GENERATED_PATHS}")


SET_SAFIR_TEST_PROPERTIES(TEST dots_cpp
  CONFIG_OVERRIDE ${CMAKE_CURRENT_SOURCE_DIR}/test_config)


if (CSHARP_FOUND)
  ADD_TEST(NAME dots_dotnet
    COMMAND ${PYTHON_EXECUTABLE} ${CMAKE_CURRENT_SOURCE_DIR}/run_test.py 
    --show-safir-config $<TARGET_FILE:safir_show_config>
    --language dotnet
    --binary $<TARGET_PROPERTY:dots_test_dotnet,ASSEMBLY_FILE>
    --dependencies $<TARGET_PROPERTY:Safir.Dob.Typesystem,ASSEMBLY_FILE>,$<TARGET_PROPERTY:dots_generated-DotsTest-dotnet,ASSEMBLY_FILE>
    --output ${CMAKE_CURRENT_SOURCE_DIR}/output.txt
    --dots-generated-paths "${DOTS_GENERATED_PATHS}"
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/dotnet)

  SET_SAFIR_TEST_PROPERTIES(TEST dots_dotnet
    CONFIG_OVERRIDE ${CMAKE_CURRENT_SOURCE_DIR}/test_config)
endif()


#if (SAFIR_ADA_SUPPORT)
#  ADD_TEST(dots_ada ${PYTHON_EXECUTABLE} ${CMAKE_CURRENT_SOURCE_DIR}/run_test.py ada)
#  SET_SAFIR_TEST_PROPERTIES(TEST dots_ada)
#endif()

#TODO: why does dots_java_jni not rebuild when make is run from here?

if (Java_FOUND)
  SAFIR_GET_JAVA_CLASSPATH(
    JAVA_TARGETS dots_test_java dots_generated-DotsTest-java dots_java logging_java)
  
  ADD_TEST(NAME dots_java
    COMMAND ${PYTHON_EXECUTABLE} ${CMAKE_CURRENT_SOURCE_DIR}/run_test.py
    --show-safir-config $<TARGET_FILE:safir_show_config>
    --language java
    --classpath ${SAFIR_JAVA_CLASSPATH}
    --output ${CMAKE_CURRENT_SOURCE_DIR}/output.txt
    --dots-generated-paths "${DOTS_GENERATED_PATHS}")

  #TODO: we should really be using the manifest instead of specifying entrypoint in the script

  SET_SAFIR_TEST_PROPERTIES(TEST dots_java
    CONFIG_OVERRIDE ${CMAKE_CURRENT_SOURCE_DIR}/test_config)

endif()
