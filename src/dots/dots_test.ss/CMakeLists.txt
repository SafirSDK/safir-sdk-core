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



#ADD_TEST(dots_dotnet ${PYTHON_EXECUTABLE} ${CMAKE_CURRENT_SOURCE_DIR}/run_test.py dotnet)

#SET_SAFIR_TEST_PROPERTIES(TEST dots_dotnet)

#if (SAFIR_ADA_SUPPORT)
#  ADD_TEST(dots_ada ${PYTHON_EXECUTABLE} ${CMAKE_CURRENT_SOURCE_DIR}/run_test.py ada)
#  SET_SAFIR_TEST_PROPERTIES(TEST dots_ada)
#endif()

#TODO: why does dots_java_jni not rebuild when make is run from here?

if (Java_FOUND)
  SAFIR_GET_JAVA_CLASSPATH_AND_LIBPATH(
    JAVA_TARGETS dots_test_java dots_generated-DotsTest-java dots_java logging_java 
    JNI_TARGETS logging_java_jni dots_java_jni)
  
  ADD_TEST(NAME dots_java
    COMMAND ${PYTHON_EXECUTABLE} ${CMAKE_CURRENT_SOURCE_DIR}/run_test.py
    --show-safir-config $<TARGET_FILE:safir_show_config>
    --language java
    --classpath ${SAFIR_JAVA_CLASSPATH}
    --library-path ${SAFIR_JAVA_LIBRARY_PATH}
    --output ${CMAKE_CURRENT_SOURCE_DIR}/output.txt
    --dots-generated-paths "${DOTS_GENERATED_PATHS}")

#TODO: we should really be using the manifest instead of specifying entrypoint in the script

SET_SAFIR_TEST_PROPERTIES(TEST dots_java CONFIG_OVERRIDE ${CMAKE_CURRENT_SOURCE_DIR}/test_config)

endif()
