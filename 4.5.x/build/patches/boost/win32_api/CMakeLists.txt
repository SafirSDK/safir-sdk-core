if (Boost_VERSION LESS 104100) #1.41
  INSTALL(FILES ${Boost_LIB_VERSION}/win32_api.hpp
    DESTINATION ${Boost_INCLUDE_DIRS}/boost/interprocess/detail/)
endif()
