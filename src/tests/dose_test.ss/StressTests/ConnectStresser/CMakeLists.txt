FILE(GLOB sources *.cpp ../common/*.cpp)
FILE(GLOB headers *.h ../common/*.h)

ADD_EXECUTABLE(ConnectStresser ${sources} ${headers})

TARGET_LINK_LIBRARIES(ConnectStresser PRIVATE
  dose_cpp
  lluf_internal
  Boost::program_options)

SAFIR_INSTALL(TARGETS ConnectStresser COMPONENT TestSuite)
