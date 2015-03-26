FILE(GLOB headers *.h)
FILE(GLOB sources *.cpp)

ADD_EXECUTABLE(dose_main ${sources} ${headers})

TARGET_LINK_LIBRARIES(dose_main PRIVATE
  dose_cpp
  dose_internal
  communication
  system_picture
  control_utils
  lluf_utils
  lluf_crash_reporter
  ${Boost_REGEX_LIBRARY})

SAFIR_INSTALL(TARGETS dose_main)
