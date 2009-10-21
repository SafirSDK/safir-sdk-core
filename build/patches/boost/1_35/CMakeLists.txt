project(boost_patches)

cmake_minimum_required(VERSION 2.4)

INSTALL(FILES file_lock.hpp
        DESTINATION ${Boost_INCLUDE_DIRS}/boost/interprocess/sync/)

INSTALL(FILES offset_ptr.hpp
        DESTINATION ${Boost_INCLUDE_DIRS}/boost/interprocess/)

INSTALL(FILES os_file_functions.hpp win32_api.hpp
        DESTINATION ${Boost_INCLUDE_DIRS}/boost/interprocess/detail/)

INSTALL(FILES sp_counted_impl.hpp
        DESTINATION ${Boost_INCLUDE_DIRS}/boost/interprocess/smart_ptr/detail)

INSTALL(FILES mapped_region.hpp
        DESTINATION ${Boost_INCLUDE_DIRS}/boost/interprocess/)

        
