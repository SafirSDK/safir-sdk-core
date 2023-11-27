#add a common executable for some of the tests here
add_executable(slow_entity_subscriber slow_entity_subscriber.cpp)

target_link_libraries(slow_entity_subscriber PRIVATE
  dose_cpp
  dose_internal
  safir_generated-DoseTest-cpp
  lluf_utils
  Boost::thread
)


add_subdirectory(basic_operations)
#add_subdirectory(unreg_reg_with_many_ghosts)
