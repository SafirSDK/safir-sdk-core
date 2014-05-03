FILE(GLOB headers *.h)
FILE(GLOB sources *.cpp)

ADD_EXECUTABLE(dose_main ${sources} ${headers})

TARGET_LINK_LIBRARIES(dose_main PRIVATE 
  dose_cpp
  dose_internal 
  dose_com 
  dots_generated-Safir-cpp
  dots_cpp
  lluf_utils
  lluf_crash_reporter
  lluf_startup_synchronizer
  ${Boost_REGEX_LIBRARY}
  ${Boost_THREAD_LIBRARY}
  ${Boost_SYSTEM_LIBRARY})

SAFIR_INSTALL(TARGETS dose_main)

#TODO: general: only the really exported headers need to be installed! e.g. dose_internal headers dont ever need to be installed!!
