FILE(GLOB_RECURSE headers *.h)
FILE(GLOB sources *.cpp)

ADD_LIBRARY(douf_time_cpp SHARED ${sources} ${headers})

target_include_directories(douf_time_cpp PUBLIC .)

TARGET_LINK_LIBRARIES(douf_time_cpp PRIVATE douf_time_library dots_generated-douf_time-cpp)

#Needed on windows
if (MSVC)
  TARGET_LINK_LIBRARIES(douf_time_cpp PRIVATE ${Boost_DATE_TIME_LIBRARY})
endif()

SAFIR_INSTALL(TARGETS douf_time_cpp INCLUDE_DIR Safir)
