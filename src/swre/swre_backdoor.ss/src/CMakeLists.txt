ADD_EXECUTABLE(backdoor main.cpp)

TARGET_LINK_LIBRARIES(backdoor PRIVATE
  dose_cpp
  ${Boost_PROGRAM_OPTIONS_LIBRARY})

#Needed on windows
if (MSVC)
  TARGET_LINK_LIBRARIES_FOOBAR(backdoor PRIVATE ${Boost_DATE_TIME_LIBRARY})
endif()

SAFIR_INSTALL(TARGETS backdoor)
