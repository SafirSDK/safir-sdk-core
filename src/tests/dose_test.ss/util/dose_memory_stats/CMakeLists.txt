FILE(GLOB sources *.cpp)

ADD_EXECUTABLE(dose_memory_stats ${sources})

TARGET_LINK_LIBRARIES(dose_memory_stats PRIVATE
  dose_internal)

SAFIR_INSTALL(TARGETS dose_memory_stats)
