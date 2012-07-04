if (Boost_VERSION LESS 104400) #1.44
  INSTALL(FILES ${Boost_LIB_VERSION}/file.hpp
    DESTINATION ${Boost_INCLUDE_DIRS}/boost/iostreams/device/)
endif()

