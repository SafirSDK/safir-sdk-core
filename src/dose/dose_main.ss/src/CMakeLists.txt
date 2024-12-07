set(headers
  BlockingHandler.h ConnectionKiller.h DoseMainApp.h MemoryMonitor.h Node.h
  PendingRegistrationHandler.h PoolDistribution.h PoolDistributionRequestSender.h
  ProcessInfoHandler.h ResponseHandler.h ConnectionHandler.h Distribution.h LockMonitor.h
  MessageHandler.h NodeInfoHandler.h PersistHandler.h PoolDistributionHandler.h
  PoolHandler.h RequestHandler.h StateDistributor.h WaitingStates.h)

set(sources
  BlockingHandler.cpp ConnectionHandler.cpp ConnectionKiller.cpp DoseMainApp.cpp
  dose_main.cpp LockMonitor.cpp MessageHandler.cpp NodeInfoHandler.cpp
  PendingRegistrationHandler.cpp PersistHandler.cpp PoolHandler.cpp
  ProcessInfoHandler.cpp RequestHandler.cpp ResponseHandler.cpp WaitingStates.cpp)

ADD_EXECUTABLE(dose_main ${sources} ${headers} PoolSyncInfo.proto)

PROTOBUF_GENERATE(TARGET dose_main OUT_VAR generated_files)

if (MSVC)
  SET_SOURCE_FILES_PROPERTIES(${generated_files} PROPERTIES COMPILE_FLAGS "/wd4127 /wd4244 /wd4267 /wd4100 /wd4702")
endif()

TARGET_LINK_LIBRARIES(dose_main PRIVATE
  dose_cpp
  dose_internal
  communication
  system_picture
  control_utils
  protobuf::protobuf
  lluf_internal
  lluf_utils
  lluf_crash_reporter
  Boost::regex)

SAFIR_INSTALL(TARGETS dose_main)
