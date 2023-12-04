set(sources
    stats_dump.cpp
    ./../../../dose/dose_dobexplorer.ss/src/ConnectionStatisticsCollector.h
    ./../../../dose/dose_dobexplorer.ss/src/ConnectionStatisticsCollector.cpp)

ADD_EXECUTABLE(dob_statistics_dump ${sources})

TARGET_LINK_LIBRARIES(dob_statistics_dump PRIVATE
  dose_internal
  Boost::program_options
  Boost::filesystem
)

SAFIR_INSTALL(TARGETS dob_statistics_dump COMPONENT Tools)

