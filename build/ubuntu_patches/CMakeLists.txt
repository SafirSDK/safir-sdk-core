project(ubuntu_patches)

cmake_minimum_required(VERSION 2.4)

INSTALL(FILES xmlada.gpr aws.gpr
        DESTINATION /usr/share/ada/adainclude/)

INSTALL(FILES file_lock.hpp
        DESTINATION /usr/include/boost/interprocess/sync/)

INSTALL(FILES offset_ptr.hpp
        DESTINATION /usr/include/boost/interprocess/)

INSTALL(FILES os_file_functions.hpp
        DESTINATION /usr/include/boost/interprocess/detail/)

INSTALL(FILES sp_counted_impl.hpp
        DESTINATION /usr/include/boost/interprocess/smart_ptr/detail)

INSTALL(FILES mapped_region.hpp
        DESTINATION /usr/include/boost/interprocess/)
