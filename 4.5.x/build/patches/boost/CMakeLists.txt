set(Boost_NO_BOOST_CMAKE ON)

find_package(Boost)

if (NOT Boost_FOUND)
   message (FATAL_ERROR "Boost was not found, cannot install boost patches!")
endif()

add_subdirectory("tmp_dir_helpers")
add_subdirectory("win32_api")
add_subdirectory("offset_ptr")
add_subdirectory("locks")
add_subdirectory("file")
add_subdirectory("sp_has_sync")
add_subdirectory("fenced_block")
