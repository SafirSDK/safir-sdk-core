if (Boost_VERSION EQUAL 104800) #1.48
  INSTALL(FILES ${Boost_LIB_VERSION}/locks.hpp
    DESTINATION ${Boost_INCLUDE_DIRS}/boost/thread/)
endif()

