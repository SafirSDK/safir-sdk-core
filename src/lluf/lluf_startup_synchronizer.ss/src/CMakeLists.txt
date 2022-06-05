#this library has an interface which is okay to call between debug/release runtimes,
#and we only want one version of it, so we never want the "d" suffix on debug builds,
#which is default on windows.
SET(CMAKE_DEBUG_POSTFIX "")

add_library(lluf_startup_synchronizer SHARED StartupSynchronizer.cpp include/Safir/Utilities/StartupSynchronizer.h)

#Make our INTERNAL headers available to other parts of Core.
target_include_directories(lluf_startup_synchronizer PUBLIC $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}/include>)

TARGET_LINK_LIBRARIES(lluf_startup_synchronizer PRIVATE
  lluf_config
  Boost::filesystem
  Boost::thread
  )

SAFIR_INSTALL(TARGETS lluf_startup_synchronizer)
