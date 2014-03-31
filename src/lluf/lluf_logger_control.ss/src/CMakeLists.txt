FILE(GLOB sources *.cpp)

ADD_EXECUTABLE(logger_control ${sources})

FIND_PACKAGE(Boost COMPONENTS filesystem program_options system)

if(NOT MSVC)
  TARGET_LINK_LIBRARIES(logger_control lluf_utils ${Boost_LIBRARIES})
endif()

INSTALL(TARGETS logger_control 
  DESTINATION ${SAFIR_RUNTIME}/bin)
