ADD_LIBRARY(lluf_utils SHARED
  DynamicLibraryLoader.cpp
  ProcessInfo.cpp
)

TARGET_LINK_LIBRARIES(lluf_utils PRIVATE
  PUBLIC
  $<BUILD_INTERFACE:Boost::filesystem>
  $<BUILD_INTERFACE:Boost::thread>
)

if (WIN32)
  TARGET_LINK_LIBRARIES(lluf_utils PRIVATE psapi)
else()
  TARGET_LINK_LIBRARIES(lluf_utils PRIVATE dl rt)
endif()

SAFIR_INSTALL(
  TARGETS lluf_utils)
