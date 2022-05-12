if (NOT Boost_FOUND)
  message(STATUS "Boost not found, will not build the VehicleAppCpp example application, since it currently requires the Boost headers.")
  return()
endif()

ADD_EXECUTABLE(VehicleAppCpp
  App.h App.cpp
  EntityHandler.h EntityHandler.cpp
  MessageSender.h MessageSender.cpp
  ServiceHandler.h ServiceHandler.cpp
  main.cpp)

TARGET_LINK_LIBRARIES(VehicleAppCpp PRIVATE
  safir_generated-Example-cpp
  ${SAFIR_SDK_CORE_LIBRARIES})

INSTALL(TARGETS VehicleAppCpp DESTINATION bin)
