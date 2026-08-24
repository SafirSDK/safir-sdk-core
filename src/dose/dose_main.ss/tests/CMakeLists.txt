ADD_SUBDIRECTORY(unit_tests)
ADD_SUBDIRECTORY(component_tests)


#this executable is useful for signalling that persistence is ready in
#tests where dope is not built yet. It is installed into the TestSuite component
#so installed system-level tests (e.g. tracer_backdoor) can use it on the PATH as
#a stand-in for dope_main.
ADD_EXECUTABLE(signal_persistence_ready signal_persistence_ready.cpp)
TARGET_LINK_LIBRARIES(signal_persistence_ready PRIVATE
  dose_cpp)

SAFIR_INSTALL(TARGETS signal_persistence_ready COMPONENT TestSuite)
