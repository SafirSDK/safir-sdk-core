ADD_SAFIR_GENERATED_LIBRARY(NAME Example DEPENDENCIES Core

  DOU_FILES
   data/Capabilities.Vehicles.GetVehicleCategoryService.dou
   data/Safir.Geodesy.Position.dou
   data/Capabilities.Vehicles.VehicleCategoryCode.dou
   data/Capabilities.Vehicles.GetVehicleCategoryResponse.dou
   data/Capabilities.Vehicles.DeleteVehicleCategoryService.dou
   data/Capabilities.Vehicles.VehicleMsg.dou
   data/Capabilities.Vehicles.SetVehicleCategoryService.dou
   data/Capabilities.Vehicles.Vehicle.dou
   data/Capabilities.SpeedObjectProperty.dou
   data/Capabilities.Vehicles.VehicleCategoryInfo.dou
   data/Capabilities.CalculateSpeedDifferenceResponse.dou
   data/Capabilities.CalculateSpeedDifference.dou
   config/Capabilities.Vehicles.DatabaseParameters.dou
   config/Capabilities.Vehicles.VehicleParameters.dou

  DOM_FILES
   data/Capabilities.Vehicles.Vehicle-Safir.Dob.InjectionProperty.dom
   data/Capabilities.Vehicles.Vehicle-Capabilities.SpeedObjectProperty.dom

  NAMESPACE_MAPPINGS
   data/Safir-java.namespace.txt
)

