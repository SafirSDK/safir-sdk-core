if(CMAKE_COMPILER_IS_GNUXX)
  INSTALL(FILES ${Boost_LIB_VERSION}/atomic.hpp
    DESTINATION ${Boost_INCLUDE_DIRS}/boost/interprocess/detail)
endif()
