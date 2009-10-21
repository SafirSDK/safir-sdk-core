project(boost_patches)

cmake_minimum_required(VERSION 2.4)

INSTALL(FILES sp_counted_impl.hpp
        DESTINATION ${Boost_INCLUDE_DIRS}/boost/interprocess/smart_ptr/detail)

INSTALL(FILES mapped_region.hpp
        DESTINATION ${Boost_INCLUDE_DIRS}/boost/interprocess/)

INSTALL(FILES win32_api.hpp
        DESTINATION ${Boost_INCLUDE_DIRS}/boost/interprocess/detail/)
