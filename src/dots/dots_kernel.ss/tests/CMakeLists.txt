add_executable(multiple_accesses multiple_accesses.cpp)
add_executable(multiple_serialization multiple_serialization.cpp)
add_executable(repository_compare repository_compare.cpp
                ../src/dots_shm_repository.h ../src/dots_shm_repository.cpp)

TARGET_LINK_LIBRARIES(multiple_accesses PRIVATE
  dots_internal
  dots_kernel
  ${Boost_THREAD_LIBRARY}
  ${Boost_SYSTEM_LIBRARY})

TARGET_LINK_LIBRARIES(multiple_serialization PRIVATE 
  dots_internal
  dots_kernel
  ${Boost_THREAD_LIBRARY}
  ${Boost_SYSTEM_LIBRARY})

TARGET_LINK_LIBRARIES(repository_compare PRIVATE
  lluf_config
  lluf_id
  lluf_utils
  dots_internal
  ${Boost_FILESYSTEM_LIBRARY})

if (NOT MSVC)
  TARGET_LINK_LIBRARIES(repository_compare PRIVATE rt)
endif()

ADD_TEST(NAME multiple_init COMMAND multiple_accesses)
ADD_TEST(NAME multiple_accesses_after_init COMMAND multiple_accesses sleep)
ADD_TEST(NAME multiple_serialization COMMAND multiple_serialization ${CMAKE_CURRENT_SOURCE_DIR}/obj.xml)
ADD_TEST(NAME repository_compare COMMAND repository_compare ${safir_sdk_core_SOURCE_DIR}/src/safir_dou)

SET_SAFIR_TEST_PROPERTIES(TEST multiple_init TIMEOUT 120)
SET_SAFIR_TEST_PROPERTIES(TEST multiple_accesses_after_init TIMEOUT 120)
SET_SAFIR_TEST_PROPERTIES(TEST multiple_serialization TIMEOUT 120)
SET_SAFIR_TEST_PROPERTIES(TEST repository_compare TIMEOUT 120)
