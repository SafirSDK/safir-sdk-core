if (Boost_VERSION GREATER 105200)
  INSTALL(FILES ${Boost_LIB_VERSION}/destroyers.hpp
    DESTINATION ${Boost_INCLUDE_DIRS}/boost/container/detail)
endif()

