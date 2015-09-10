if (UNIX)
  set(DEFAULT_FILE_PERSISTENCE_DIRECTORY /var/lib/safir-sdk-core/persistence/)
else()
  set(DEFAULT_FILE_PERSISTENCE_DIRECTORY @{CSIDL_COMMON_APPDATA}/safir-sdk-core/persistence)
endif()

if (UNIX)
  set(DEFAULT_INCARNATION_BLACKLIST /var/lib/safir-sdk-core/system-incarnation-blacklist.txt)
else()
  set(DEFAULT_INCARNATION_BLACKLIST @{CSIDL_COMMON_APPDATA}/safir-sdk-core/system-incarnation-blacklist.txt)
endif()

#We configure the file into the source dir, which may seem a bit strange
#for out-of-source builds, but we need all the dou files in one directory
#since our tests point there for dou file loading.
configure_file(Safir.Dob.PersistenceParameters.dou.in
  "${CMAKE_CURRENT_SOURCE_DIR}/config/Safir.Dob.PersistenceParameters.dou" @ONLY)

configure_file(Safir.Dob.NodeParameters.dou.in
  "${CMAKE_CURRENT_SOURCE_DIR}/config/Safir.Dob.NodeParameters.dou" @ONLY)


ADD_SAFIR_GENERATED_LIBRARY(NAME Core

  DOU_FILES

  "${CMAKE_CURRENT_SOURCE_DIR}/config/Safir.Dob.PersistenceParameters.dou"
  config/Safir.Dob.NodeParameters.dou
  config/Safir.Dob.QueueParameters.dou
  config/Safir.Dob.ThisNodeParameters.dou
  config/Safir.Time.LibraryParameters.dou
  data/Safir.Application.BackdoorCommand.dou
  data/Safir.Control.Command.dou
  data/Safir.Control.Operation.dou
  data/Safir.Control.Status.dou
  data/Safir.Databases.Odbc.IntegrityConstraintException.dou
  data/Safir.Databases.Odbc.ReconnectException.dou
  data/Safir.Databases.Odbc.RetryException.dou
  data/Safir.Databases.Odbc.TimeoutException.dou
  data/Safir.Dob.AccessDeniedException.dou
  data/Safir.Dob.CallbackId.dou
  data/Safir.Dob.ConnectionInfo.dou
  data/Safir.Dob.ConnectionQueueId.dou
  data/Safir.Dob.ContextSharedOverrideProperty.dou
  data/Safir.Dob.ContextSharedProperty.dou
  data/Safir.Dob.CreateRoutineParameters.dou
  data/Safir.Dob.DistributionScope.dou
  data/Safir.Dob.DistributionScopeProperty.dou
  data/Safir.Dob.DistributionScopeOverrideProperty.dou
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
  data/Safir.Dob.NodeType.dou
  data/Safir.Dob.NotFoundException.dou
  data/Safir.Dob.NotOpenException.dou
  data/Safir.Dob.OverflowException.dou
  data/Safir.Dob.Parametrization.dou
  data/Safir.Dob.PersistenceBackend.dou
  data/Safir.Dob.PersistenceThrottlingOverrideProperty.dou
  data/Safir.Dob.PersistenceThrottlingProperty.dou
  data/Safir.Dob.PersistentDataReady.dou
  data/Safir.Dob.PersistentDataState.dou
  data/Safir.Dob.PersistentDataStatus.dou
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
  data/Safir.Application.BackdoorCommand-Safir.Dob.ContextSharedProperty.dom
  data/Safir.Control.Command-Safir.Dob.DistributionScopeProperty.dom
  data/Safir.Control.Status-Safir.Dob.DistributionScopeProperty.dom
  data/Safir.Dob.PersistentDataReady-Safir.Dob.DistributionScopeProperty.dom
  data/Safir.Dob.ProcessInfo-Safir.Dob.DistributionScopeProperty.dom
  data/Safir.Utilities.ForEach.DeleteAllRequest-Safir.Dob.DistributionScopeProperty.dom
  data/Safir.Utilities.ForEach.DeleteRequest-Safir.Dob.DistributionScopeProperty.dom
  data/Safir.Utilities.ForEach.UpdateRequest-Safir.Dob.DistributionScopeProperty.dom

  NAMESPACE_MAPPINGS

  config/Safir-java.namespace.txt
)

INSTALL_SAFIR_GENERATED_LIBRARY(TARGETS Core)
