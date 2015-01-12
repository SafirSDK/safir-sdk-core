if (NOT Java_FOUND)
  return()
endif()

file(GLOB_RECURSE sources com/*.java)

ADD_JAR(vehicleappjava
  SOURCES ${sources}
  INCLUDE_JARS ${SAFIR_SDK_CORE_JAVA_DIR}/logging_java.jar ${SAFIR_SDK_CORE_JAVA_DIR}/dose_java.jar safir_generated-Example-java
  ENTRY_POINT com.saabgroup.safir.samples.vehicleappjava.VehicleAppJava
  MANIFEST ${CMAKE_CURRENT_SOURCE_DIR}/Manifest.txt)


