cmake_minimum_required(VERSION 2.8)

project(TestDoseCpp CXX)

INCLUDE($ENV{SAFIR_SDK}/data/build/cmake_settings.txt)

ADD_EXECUTABLE(consumer_cast_test consumer_cast_test.cpp)
TARGET_LINK_LIBRARIES(consumer_cast_test dots_cpp)

ENABLE_TESTING()
ADD_TEST(ConsumerCasting consumer_cast_test)
