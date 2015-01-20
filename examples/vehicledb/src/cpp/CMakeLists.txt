ADD_EXECUTABLE(VehicleAppDb
  App.h App.cpp
  DatabaseInteraction.h DatabaseInteraction.cpp
  VehicleDatabaseServices.h VehicleDatabaseServices.cpp
  main.cpp)

TARGET_LINK_LIBRARIES(VehicleAppDb
  safir_generated-Example-cpp
  ${SAFIR_SDK_CORE_LIBRARIES})
