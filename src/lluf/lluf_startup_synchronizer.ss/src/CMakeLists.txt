#this library has an interface which is okay to call between debug/release runtimes,
#and we only want one version of it, so we never want the "d" suffix on debug builds,
#which is default on windows.
SET(CMAKE_DEBUG_POSTFIX "")

add_library(lluf_startup_synchronizer SHARED StartupSynchronizer.cpp)

TARGET_LINK_LIBRARIES(lluf_startup_synchronizer PRIVATE
  lluf_config
  Boost::filesystem
  Boost::thread
  )

SAFIR_INSTALL(TARGETS lluf_startup_synchronizer)
