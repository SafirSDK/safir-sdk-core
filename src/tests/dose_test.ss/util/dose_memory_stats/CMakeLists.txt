FILE(GLOB sources *.cpp)

ADD_EXECUTABLE(dose_memory_stats ${sources})

TARGET_LINK_LIBRARIES(dose_memory_stats PRIVATE 
  dose_internal 
#  dots_cpp 
#  dots_internal
#  lluf_utils
#  lluf_startup_synchronizer
  ${Boost_THREAD_LIBRARY} 
  ${Boost_SYSTEM_LIBRARY})

SAFIR_INSTALL(TARGETS dose_memory_stats)
