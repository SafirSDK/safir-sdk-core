ADD_SUBDIRECTORY(unit_tests)
ADD_SUBDIRECTORY(component_tests)


#this executable is useful for signalling that persistence is ready in
#tests where dope is not built yet.
ADD_EXECUTABLE(signal_persistence_ready signal_persistence_ready.cpp)
TARGET_LINK_LIBRARIES(signal_persistence_ready PRIVATE
  dose_cpp)
