if (Boost_VERSION LESS 105700 AND #1.57
    INSTALL(FILES ${Boost_LIB_VERSION}/atomic.hpp
      DESTINATION ${Boost_INCLUDE_DIRS}/boost/interprocess/detail)
endif()
