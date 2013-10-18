FILE(GLOB headers *.h)
FILE(GLOB sources *.cpp)
ADD_EXECUTABLE(dots_shmem_repo ${sources} ${headers})

if (NOT MSVC)
    TARGET_LINK_LIBRARIES(dots_shmem_repo dots_id lluf_utils lluf_startup_synchronizer dots_internal ${Boost_LIBRARIES} rt)
endif()

add_test(dots_shm_test dots_shmem_repo -c ${CMAKE_CURRENT_SOURCE_DIR}/../serialization_test/dou)
