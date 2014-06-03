if (NOT CSHARP_FOUND)
  return()
endif()



ADD_CSHARP_ASSEMBLY(consumer_handling_test EXE 
  SOURCES consumer_handling_test.cs ../src/ConsumerHandler.cs ../src/Consumer.cs ../src/ConsumerBase.cs ../src/EntityRequestProxy.cs ../src/ResponseSender.cs ../src/InjectedEntityProxy.cs ../src/ServiceRequestProxy.cs ../src/ResponseProxy.cs ../src/MessageProxy.cs ../src/EntityProxy.cs ../src/PreviousEntityProxy.cs ../src/Interface.cs
  REFERENCES dots_generated-Safir-dotnet Safir.Dob.Typesystem Safir.Logging)

#ADD_TEST(NAME logging_dotnet
#  COMMAND ${PYTHON_EXECUTABLE} ${CMAKE_CURRENT_SOURCE_DIR}/run_test.py
#  --safir-show-config $<TARGET_FILE:safir_show_config>
#  --sender-exe $<TARGET_PROPERTY:log_sender_dotnet,ASSEMBLY_FILE>
#  --dependencies $<TARGET_PROPERTY:Safir.Logging,ASSEMBLY_FILE>)

#SET_SAFIR_TEST_PROPERTIES(TEST logging_dotnet TIMEOUT 120)

