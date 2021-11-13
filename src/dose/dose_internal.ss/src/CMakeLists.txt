FILE(GLOB_RECURSE headers *.h)
FILE(GLOB sources *.cpp)

ADD_LIBRARY(dose_internal SHARED ${sources} ${headers})

#Make our INTERNAL headers available to other parts of Core.
target_include_directories(dose_internal PUBLIC $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}/include>)

TARGET_LINK_LIBRARIES(dose_internal PRIVATE
  lluf_id
  lluf_utils
  lluf_startup_synchronizer
  Boost::regex

  PUBLIC
  $<BUILD_INTERFACE:safir_generated-Core-cpp>)

#These libraries are used in our internal headers, not in the public headers so we
#only make them public for the Core tree build, not for the exported cmake
#configuration. This allows for other parts of core to include dose_internal things
#without having to add thread and system to their link dependencies.
#The reason for having different code on windows and Linux is a bug in cmake, which
#makes the correct code (which is in the Linux branch) not work on Windows.
if (MSVC)
  TARGET_LINK_LIBRARIES(dose_internal PRIVATE
    PUBLIC
    Boost::thread
    Boost::system)
else()
  TARGET_LINK_LIBRARIES(dose_internal PRIVATE
    PUBLIC
    "$<BUILD_INTERFACE:Boost::thread>"
    "$<BUILD_INTERFACE:Boost::system>")
endif()

if (UNIX)
  TARGET_LINK_LIBRARIES(dose_internal PRIVATE rt)
endif()



SAFIR_INSTALL(TARGETS dose_internal)
