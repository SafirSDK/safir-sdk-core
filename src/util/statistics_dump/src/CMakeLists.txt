set(sources
    stats_dump.cpp
    ./../../../dose/dose_dobexplorer.ss/src/ConnectionStatisticsCollector.h
    ./../../../dose/dose_dobexplorer.ss/src/ConnectionStatisticsCollector.cpp)

ADD_EXECUTABLE(safir_statistics_dump ${sources})

TARGET_LINK_LIBRARIES(safir_statistics_dump PRIVATE
  dose_internal
  Boost::program_options
  Boost::filesystem
)

SAFIR_INSTALL(TARGETS safir_statistics_dump COMPONENT Tools)

