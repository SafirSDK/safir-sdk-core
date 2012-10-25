cmake_minimum_required(VERSION 2.8)

project(OlibTester CXX)

enable_testing()

INCLUDE_DIRECTORIES(.)

INCLUDE($ENV{SAFIR_SDK}/data/build/cmake_settings.txt)

FILE(GLOB_RECURSE headers *.h)
FILE(GLOB sources *.cpp)
add_executable(OlibTester ${sources} ${headers})

if (NOT MSVC)
    TARGET_LINK_LIBRARIES(OlibTester  olibd)
else()
    TARGET_LINK_LIBRARIES(OlibTester  olibd)
endif()

ADD_TEST(OlibTest_Connect OlibTester -connect)
ADD_TEST(OlibTest_Disconnect OlibTester -disconnect)
ADD_TEST(OlibTest_ClearTable OlibTester -cleartable)
ADD_TEST(OlibTest_CreateUnit OlibTester -createunit)
ADD_TEST(OlibTest_ReadUnit   OlibTester -readunit)
ADD_TEST(OlibTest_UpdateUnit   OlibTester -updateunit)
ADD_TEST(OlibTest_DeleteUnit   OlibTester -deleteunit)
ADD_TEST(OlibTest_BinaryWriteRead   OlibTester -binaryrw)
ADD_TEST(OlibTest_BlobWriteRead   OlibTester -blobrw)
ADD_TEST(OlibTest_NClobWriteRead   OlibTester -nclobrw)
ADD_TEST(OlibTest_LotsOfInput   OlibTester -lotsofinput)


