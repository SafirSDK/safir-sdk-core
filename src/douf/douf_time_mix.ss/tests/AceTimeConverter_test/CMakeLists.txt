
ADD_EXECUTABLE(AceTimeConverter_test test.cpp)

if (NOT MSVC)
  TARGET_LINK_LIBRARIES(AceTimeConverter_test douf_time_cpp)
endif()

TARGET_LINK_LIBRARIES(AceTimeConverter_test ${ACE_LIBRARY})

ADD_TEST(AceTimeConverter_test AceTimeConverter_test)
