if (Boost_VERSION LESS 105000 AND #1.50
    CMAKE_SYSTEM_PROCESSOR MATCHES "^armv6" OR
    CMAKE_SYSTEM_PROCESSOR MATCHES "^armv7")
  INSTALL(FILES ${Boost_LIB_VERSION}/spinlock_gcc_arm.hpp
    DESTINATION ${Boost_INCLUDE_DIRS}/boost/smart_ptr/detail)
endif()

