set(sources memory_allocator.cpp)

ADD_EXECUTABLE(safir_memory_allocator ${sources})

TARGET_LINK_LIBRARIES(safir_memory_allocator PRIVATE
  dose_internal
  Boost::program_options
)

SAFIR_INSTALL(TARGETS safir_memory_allocator COMPONENT Tools)

