if (NOT CSHARP_FOUND)
  return()
endif()


ADD_CSHARP_ASSEMBLY(consumer_handling_test EXE 
  SOURCES consumer_handling_test.cs ../src/ConsumerHandler.cs ../src/Consumer.cs ../src/ConsumerBase.cs ../src/EntityRequestProxy.cs ../src/ResponseSender.cs ../src/InjectedEntityProxy.cs ../src/ServiceRequestProxy.cs ../src/ResponseProxy.cs ../src/MessageProxy.cs ../src/EntityProxy.cs ../src/PreviousEntityProxy.cs ../src/Interface.cs
  REFERENCES dots_generated-Safir-dotnet Safir.Dob.Typesystem Safir.Logging)

ADD_TEST(NAME consumer_handling_test
  COMMAND $<TARGET_PROPERTY:consumer_handling_test,ASSEMBLY_FILE>)

SET_SAFIR_TEST_PROPERTIES(TEST consumer_handling_test TIMEOUT 120)

