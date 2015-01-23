ADD_EXECUTABLE(VehicleAppCpp
  App.h App.cpp
  EntityHandler.h EntityHandler.cpp
  MessageSender.h MessageSender.cpp
  ServiceHandler.h ServiceHandler.cpp
  main.cpp)

TARGET_LINK_LIBRARIES(VehicleAppCpp PRIVATE
  safir_generated-Example-cpp
  ${SAFIR_SDK_CORE_LIBRARIES})
