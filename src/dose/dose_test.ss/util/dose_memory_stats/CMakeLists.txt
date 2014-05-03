FILE(GLOB sources *.cpp)

ADD_EXECUTABLE(dose_memory_stats ${sources})

#FIND_PACKAGE(Boost COMPONENTS system thread)

TARGET_LINK_LIBRARIES(dose_memory_stats PRIVATE 
  dose_internal 
  dots_cpp 
  dots_internal
  lluf_utils
  lluf_startup_synchronizer
  ${Boost_THREAD_LIBRARY} 
  ${Boost_SYSTEM_LIBRARY})

# INSTALL(TARGETS dose_memory_stats
#         RUNTIME DESTINATION ${SAFIR_RUNTIME}/bin
#         LIBRARY DESTINATION ${SAFIR_RUNTIME}/lib
#         ARCHIVE DESTINATION ${SAFIR_SDK}/lib)

# INSTALL_DEBUG_INFO(dose_memory_stats)
