if (Boost_FOUND)
  add_definitions(-DVEHICLEAPP_USE_BOOST)
else()
  message(STATUS "Boost not found, will use alternative implementation")
  add_definitions(-DSAFIR_NO_BOOST)
endif()

ADD_EXECUTABLE(VehicleAppCpp
  App.h App.cpp
  EntityHandler.h EntityHandler.cpp
  MessageSender.h MessageSender.cpp
  ServiceHandler.h ServiceHandler.cpp
  main.cpp)

TARGET_LINK_LIBRARIES(VehicleAppCpp PRIVATE
  safir_generated-Example-cpp
  ${SAFIR_SDK_CORE_LIBRARIES}
  Boost::boost)

INSTALL(TARGETS VehicleAppCpp DESTINATION bin)
