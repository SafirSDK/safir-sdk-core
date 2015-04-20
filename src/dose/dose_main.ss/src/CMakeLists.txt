FILE(GLOB headers
  BlockingHandler.h ConnectionKiller.h DoseMainApp.h MemoryMonitor.h Node.h
  PendingRegistrationHandler.h PoolDistribution.h PoolDistributionRequestSender.h
  ProcessInfoHandler.h ResponseHandler.h ConnectionHandler.h Distribution.h LockMonitor.h
  MessageHandler.h NodeInfoHandler.h PersistHandler.h PoolDistributionHandler.h
  PoolHandler.h RequestHandler.h StateDistributor.h)

FILE(GLOB sources
  BlockingHandler.cpp ConnectionHandler.cpp ConnectionKiller.cpp DoseMainApp.cpp
  dose_main.cpp LockMonitor.cpp MessageHandler.cpp NodeInfoHandler.cpp
  PendingRegistrationHandler.cpp PersistHandler.cpp PoolHandler.cpp
  ProcessInfoHandler.cpp RequestHandler.cpp ResponseHandler.cpp)

ADD_EXECUTABLE(dose_main ${sources} ${headers})

TARGET_LINK_LIBRARIES(dose_main PRIVATE
  dose_cpp
  dose_internal
  communication
  system_picture
  control_utils
  lluf_utils
  lluf_crash_reporter
  ${Boost_REGEX_LIBRARY})

SAFIR_INSTALL(TARGETS dose_main)
