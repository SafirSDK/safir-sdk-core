if (Boost_VERSION LESS 104800) #1.48
  INSTALL(FILES ${Boost_LIB_VERSION}/tmp_dir_helpers.hpp
    DESTINATION ${Boost_INCLUDE_DIRS}/boost/interprocess/detail/)
elseif(Boost_VERSION GREATER 104900 AND #1.49 < version < 1.57
    Boost_VERSION LESS 105700)
  INSTALL(FILES ${Boost_LIB_VERSION}/workaround.hpp
    DESTINATION ${Boost_INCLUDE_DIRS}/boost/interprocess/detail/)
endif()
