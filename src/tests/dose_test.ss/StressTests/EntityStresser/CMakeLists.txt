FILE(GLOB sources *.cpp ../common/*.cpp)
FILE(GLOB headers *.h ../common/*.h)

ADD_EXECUTABLE(EntityStresser ${sources} ${headers})

TARGET_LINK_LIBRARIES(EntityStresser PRIVATE
  dose_cpp
  safir_generated-DoseTest-cpp
  lluf_utils
  Boost::program_options)

SAFIR_INSTALL(TARGETS EntityStresser COMPONENT TestSuite)
