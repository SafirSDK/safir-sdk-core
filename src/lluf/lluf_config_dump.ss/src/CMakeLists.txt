FILE(GLOB sources *.cpp)

ADD_EXECUTABLE(safir_show_config ${sources})

FIND_PACKAGE(Boost COMPONENTS program_options)

#add_dependencies(safir_show_config lluf_config)

if(NOT MSVC)
  TARGET_LINK_LIBRARIES(safir_show_config lluf_config ${Boost_LIBRARIES})
endif()

INSTALL(TARGETS safir_show_config  
  DESTINATION ${SAFIR_RUNTIME}/bin)
