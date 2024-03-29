ADD_SUBDIRECTORY(cpp)
ADD_SUBDIRECTORY(dotnet)
ADD_SUBDIRECTORY(java)

ADD_TEST(NAME dots_cpp
  COMMAND ${Python3_EXECUTABLE} ${CMAKE_CURRENT_SOURCE_DIR}/run_test.py
  --show-safir-config $<TARGET_FILE:safir_show_config>
  --language cpp
  --binary $<TARGET_FILE:dots_test_cpp>
  --output ${CMAKE_CURRENT_SOURCE_DIR}/output.txt)


SET_SAFIR_TEST_PROPERTIES(TEST dots_cpp
  CONFIG_OVERRIDE ${CMAKE_CURRENT_SOURCE_DIR}/test_config)


if (CSHARP_FOUND)
  ADD_TEST(NAME dots_dotnet
    COMMAND ${Python3_EXECUTABLE} ${CMAKE_CURRENT_SOURCE_DIR}/run_test.py
    --show-safir-config $<TARGET_FILE:safir_show_config>
    --language dotnet
    --binary $<TARGET_GENEX_EVAL:dots_test_dotnet,$<TARGET_PROPERTY:dots_test_dotnet,ASSEMBLY_FILE>>
    --output ${CMAKE_CURRENT_SOURCE_DIR}/output.txt
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/dotnet)

  SET_SAFIR_TEST_PROPERTIES(TEST dots_dotnet
    CONFIG_OVERRIDE ${CMAKE_CURRENT_SOURCE_DIR}/test_config)
endif()

if (Java_FOUND)
  ADD_TEST(NAME dots_java
    COMMAND ${Python3_EXECUTABLE} ${CMAKE_CURRENT_SOURCE_DIR}/run_test.py
    --show-safir-config $<TARGET_FILE:safir_show_config>
    --language java
    --jar $<TARGET_PROPERTY:dots_test_java,JAR_FILE>
    --output ${CMAKE_CURRENT_SOURCE_DIR}/output.txt
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/java)

  SET_SAFIR_TEST_PROPERTIES(TEST dots_java
    CONFIG_OVERRIDE ${CMAKE_CURRENT_SOURCE_DIR}/test_config)

endif()
