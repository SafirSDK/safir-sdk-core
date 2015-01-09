ADD_LIBRARY(douf_time_cpp SHARED TimeProvider.cpp)

TARGET_LINK_LIBRARIES(douf_time_cpp PRIVATE
  douf_time_library
  dots_cpp)

#Needed on windows
if (MSVC)
  TARGET_LINK_LIBRARIES(douf_time_cpp PRIVATE ${Boost_DATE_TIME_LIBRARY})
endif()

SAFIR_INSTALL(TARGETS douf_time_cpp)
