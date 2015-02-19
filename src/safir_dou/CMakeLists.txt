if (UNIX)
  set(DEFAULT_FILE_PERSISTENCE_DIRECTORY /var/lib/safir-sdk-core/persistence/)
else()
  set(DEFAULT_FILE_PERSISTENCE_DIRECTORY TODO)
endif()

configure_file(Safir.Dob.PersistenceParameters.dou.in
  "${CMAKE_CURRENT_BINARY_DIR}/config/Safir.Dob.PersistenceParameters.dou" @ONLY)

ADD_SAFIR_GENERATED_LIBRARY(NAME Core

  DOU_FILES

  "${CMAKE_CURRENT_BINARY_DIR}/config/Safir.Dob.PersistenceParameters.dou"
  config/Safir.Dob.DistributionChannelParameters.dou
  config/Safir.Dob.NodeParameters.dou
  config/Safir.Dob.QueueParameters.dou
  config/Safir.Dob.ThisNodeParameters.dou
  config/Safir.Time.LibraryParameters.dou
  data/Safir.Application.BackdoorCommand.dou
  data/Safir.Databases.Odbc.IntegrityConstraintException.dou
  data/Safir.Databases.Odbc.ReconnectException.dou
  data/Safir.Databases.Odbc.RetryException.dou
  data/Safir.Databases.Odbc.TimeoutException.dou
  data/Safir.Dob.AccessDeniedException.dou
  data/Safir.Dob.Alias.dou
  data/Safir.Dob.CallbackId.dou
  data/Safir.Dob.ConnectionInfo.dou
  data/Safir.Dob.ConnectionQueueId.dou
  data/Safir.Dob.ContextSharedOverrideProperty.dou
  data/Safir.Dob.ContextSharedProperty.dou
  data/Safir.Dob.CreateRoutineParameters.dou
  data/Safir.Dob.DistributionChannelAliases.dou
  data/Safir.Dob.DistributionChannel.dou
  data/Safir.Dob.DistributionChannelOverrideProperty.dou
  data/Safir.Dob.DistributionChannelProperty.dou
  data/Safir.Dob.Entity.dou
  data/Safir.Dob.EntityIdResponse.dou
  data/Safir.Dob.ErrorListResponse.dou
  data/Safir.Dob.ErrorResponse.dou
  data/Safir.Dob.GhostExistsException.dou
  data/Safir.Dob.InjectionKind.dou
  data/Safir.Dob.InjectionOverrideProperty.dou
  data/Safir.Dob.InjectionProperty.dou
  data/Safir.Dob.InstanceIdPolicy.dou
  data/Safir.Dob.Item.dou
  data/Safir.Dob.Message.dou
  data/Safir.Dob.NodeDefinition.dou
  data/Safir.Dob.NodeInfo.dou
  data/Safir.Dob.NodeStatus.dou
  data/Safir.Dob.NotFoundException.dou
  data/Safir.Dob.NotOpenException.dou
  data/Safir.Dob.OverflowException.dou
  data/Safir.Dob.Parametrization.dou
  data/Safir.Dob.PersistenceBackend.dou
  data/Safir.Dob.PersistentDataReady.dou
  data/Safir.Dob.PersistentDataState.dou
  data/Safir.Dob.PersistentDataStatus.dou
  data/Safir.Dob.PriorityAliasParameters.dou
  data/Safir.Dob.Priority.dou
  data/Safir.Dob.PriorityOverrideProperty.dou
  data/Safir.Dob.PriorityParameters.dou
  data/Safir.Dob.PriorityProperty.dou
  data/Safir.Dob.ProcessInfo.dou
  data/Safir.Dob.QueueRule.dou
  data/Safir.Dob.RequestTimeoutOverrideProperty.dou
  data/Safir.Dob.RequestTimeoutProperty.dou
  data/Safir.Dob.Response.dou
  data/Safir.Dob.ResponseErrorInfo.dou
  data/Safir.Dob.ResponseGeneralErrorCodes.dou
  data/Safir.Dob.Service.dou
  data/Safir.Dob.Struct.dou
  data/Safir.Dob.SuccessResponse.dou
  data/Safir.Utilities.ForEach.BriefResponse.dou
  data/Safir.Utilities.ForEach.DeleteAllRequest.dou
  data/Safir.Utilities.ForEach.DeleteRequest.dou
  data/Safir.Utilities.ForEach.FullResponse.dou
  data/Safir.Utilities.ForEach.ResponseType.dou
  data/Safir.Utilities.ForEach.UpdateRequest.dou

  DOM_FILES

  config/Safir.Dob.Entity-Safir.Dob.RequestTimeoutProperty.dom
  config/Safir.Dob.Service-Safir.Dob.RequestTimeoutProperty.dom
  config/Safir.Utilities.ForEach.DeleteAllRequest-Safir.Dob.RequestTimeoutProperty.dom
  config/Safir.Utilities.ForEach.DeleteRequest-Safir.Dob.RequestTimeoutProperty.dom
  config/Safir.Utilities.ForEach.UpdateRequest-Safir.Dob.RequestTimeoutProperty.dom
  data/Object-Safir.Dob.DistributionChannelProperty.dom
  data/Object-Safir.Dob.PriorityProperty.dom
  data/Safir.Application.BackdoorCommand-Safir.Dob.ContextSharedProperty.dom
  data/Safir.Dob.NodeInfo-Safir.Dob.DistributionChannelProperty.dom
  data/Safir.Dob.PersistentDataReady-Safir.Dob.DistributionChannelProperty.dom
  data/Safir.Dob.ProcessInfo-Safir.Dob.DistributionChannelProperty.dom
  data/Safir.Utilities.ForEach.DeleteAllRequest-Safir.Dob.DistributionChannelProperty.dom
  data/Safir.Utilities.ForEach.DeleteRequest-Safir.Dob.DistributionChannelProperty.dom
  data/Safir.Utilities.ForEach.UpdateRequest-Safir.Dob.DistributionChannelProperty.dom

  NAMESPACE_MAPPINGS

  config/Safir-java.namespace.txt
)

INSTALL_SAFIR_GENERATED_LIBRARY(TARGETS Core)
