if (Boost_VERSION LESS 104800) #1.48
  INSTALL(FILES ${Boost_LIB_VERSION}/offset_ptr.hpp
    DESTINATION ${Boost_INCLUDE_DIRS}/boost/interprocess/)
endif()
