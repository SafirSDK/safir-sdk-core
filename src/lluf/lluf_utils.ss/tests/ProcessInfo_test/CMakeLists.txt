PROJECT (ProcessInfo_test)
INCLUDE_DIRECTORIES($ENV{SAFIR_SDK}/include)
LINK_DIRECTORIES($ENV{SAFIR_RUNTIME}/lib)
FILE(GLOB sources *.cpp)

ADD_EXECUTABLE(ProcessInfo_test ${sources})
TARGET_LINK_LIBRARIES (ProcessInfo_test ACE lluf_utils)

ADD_TEST(ProcessInfo ProcessInfo_test)
