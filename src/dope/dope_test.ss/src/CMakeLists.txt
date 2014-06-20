ADD_EXECUTABLE(entity_owner entity_owner.cpp)

TARGET_LINK_LIBRARIES(entity_owner PRIVATE
  dose_cpp
  safir_generated-DopeTest-cpp
  ${Boost_SYSTEM_LIBRARY})

add_subdirectory(file_backend)
add_subdirectory(odbc_backend)

#TODO: install when installing test suite
