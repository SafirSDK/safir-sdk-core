#
# NOTE: Olib is deprecated, but we still provide this example program.
#       Please don't start using Olib now!
#

add_definitions(-DSAFIR_DISABLE_OLIB_DEPRECATION_WARNING)


ADD_EXECUTABLE(VehicleAppDb
  App.h App.cpp
  DatabaseInteraction.h DatabaseInteraction.cpp
  VehicleDatabaseServices.h VehicleDatabaseServices.cpp
  main.cpp)

TARGET_LINK_LIBRARIES(VehicleAppDb
  safir_generated-Example-cpp
  ${SAFIR_SDK_CORE_OLIB_LIBRARIES}
  ${SAFIR_SDK_CORE_LIBRARIES})
