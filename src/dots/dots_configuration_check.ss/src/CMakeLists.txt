FILE(GLOB sources *.cpp)

ADD_EXECUTABLE(dots_configuration_check ${sources})

TARGET_LINK_LIBRARIES(dots_configuration_check PRIVATE
  dots_kernel
  dots_internal
  ${Boost_PROGRAM_OPTIONS_LIBRARY})

SAFIR_INSTALL(TARGETS dots_configuration_check)
