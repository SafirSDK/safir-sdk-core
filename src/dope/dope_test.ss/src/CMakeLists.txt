ADD_EXECUTABLE(entity_owner entity_owner.cpp)

TARGET_LINK_LIBRARIES(entity_owner PRIVATE
  dose_cpp
  safir_generated-DopeTest-cpp)

SAFIR_INSTALL(TARGETS entity_owner TEST_SUITE)

#TODO: stewart  readd these when persisthandler works
#add_subdirectory(none_backend)
#add_subdirectory(file_backend)
add_subdirectory(odbc_backend)
