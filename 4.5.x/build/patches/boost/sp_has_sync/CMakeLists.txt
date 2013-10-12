if (Boost_VERSION LESS 105300 AND #1.53 TODO check that it is fixed in 1.53
    (CMAKE_SYSTEM_PROCESSOR MATCHES "^arm" OR
     CMAKE_SYSTEM_PROCESSOR MATCHES "^armv7"))
  INSTALL(FILES ${Boost_LIB_VERSION}/sp_has_sync.hpp
    DESTINATION ${Boost_INCLUDE_DIRS}/boost/smart_ptr/detail)
endif()

