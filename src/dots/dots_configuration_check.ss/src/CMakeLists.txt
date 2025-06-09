ADD_EXECUTABLE(dots_configuration_check dots_configuration_check.cpp dou_diff_helper.h)

TARGET_LINK_LIBRARIES(dots_configuration_check PRIVATE
  lluf_internal
  dots_kernel
  dots_internal
  #dots_cpp
  Boost::program_options)

SAFIR_INSTALL(TARGETS dots_configuration_check)
