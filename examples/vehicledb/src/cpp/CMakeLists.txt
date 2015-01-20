ADD_EXECUTABLE(VehicleAppDb
  App.h App.cpp
  DatabaseInteraction.h DatabaseInteraction.cpp
  VehicleDatabaseServices.h VehicleDatabaseServices.cpp
  main.cpp)

#TODO: remove ACE support
ADD_DEFINITIONS(-DNO_ACE)

TARGET_LINK_LIBRARIES(VehicleAppDb
  safir_generated-Example-cpp
  ${SAFIR_SDK_CORE_LIBRARIES})
