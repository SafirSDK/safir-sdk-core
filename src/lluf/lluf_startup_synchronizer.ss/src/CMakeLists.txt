add_library(lluf_startup_synchronizer SHARED StartupSynchronizer.cpp include/Safir/Utilities/StartupSynchronizer.h)
safir_mark_single_abi(lluf_startup_synchronizer)

#Make our INTERNAL headers available to other parts of Core.
target_include_directories(lluf_startup_synchronizer PUBLIC $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}/include>)

TARGET_LINK_LIBRARIES(lluf_startup_synchronizer PRIVATE
  lluf_config
  Boost::filesystem
  Boost::thread
  )

SAFIR_INSTALL(TARGETS lluf_startup_synchronizer)
