project(boost_patches)

cmake_minimum_required(VERSION 2.4)

INSTALL(FILES win32_api.hpp
        DESTINATION ${Boost_INCLUDE_DIRS}/boost/interprocess/detail/)
