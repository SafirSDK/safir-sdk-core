FILE(GLOB headers *.h)
FILE(GLOB sources *.cpp)

ADD_EXECUTABLE(dose_main ${sources} ${headers})

TARGET_LINK_LIBRARIES(dose_main PRIVATE
  dose_cpp
  dose_internal
  dose_com
  dots_cpp
  lluf_crash_reporter
  ${Boost_REGEX_LIBRARY}
  ${Boost_THREAD_LIBRARY}
  ${Boost_SYSTEM_LIBRARY})

SAFIR_INSTALL(TARGETS dose_main)
