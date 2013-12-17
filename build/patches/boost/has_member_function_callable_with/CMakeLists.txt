if (Boost_VERSION EQUAL 105500) #1.55
  INSTALL(FILES ${Boost_LIB_VERSION}/has_member_function_callable_with.hpp
    DESTINATION ${Boost_INCLUDE_DIRS}/boost/intrusive/detail/)
endif()

