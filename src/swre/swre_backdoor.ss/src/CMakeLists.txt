ADD_EXECUTABLE(bd main.cpp)

TARGET_LINK_LIBRARIES(bd PRIVATE
  dose_cpp
  Boost::program_options)

SAFIR_INSTALL(TARGETS bd COMPONENT Tools)
