ADD_EXECUTABLE(VehicleAppCpp
  App.h App.cpp
  EntityHandler.h EntityHandler.cpp
  MessageSender.h MessageSender.cpp
  ServiceHandler.h ServiceHandler.cpp
  main.cpp)

#TODO: remove ACE from the code
ADD_DEFINITIONS(-DNO_ACE)

TARGET_LINK_LIBRARIES(VehicleAppCpp safir_generated-Example-cpp ${SAFIR_SDK_CORE_LIBRARIES} ${Boost_LIBRARIES})

#TODO: install?
