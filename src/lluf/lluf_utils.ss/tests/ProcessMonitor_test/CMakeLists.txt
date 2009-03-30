PROJECT (ProcessMonitor_test)
INCLUDE_DIRECTORIES($ENV{SAFIR_SDK}/include)
LINK_DIRECTORIES($ENV{SAFIR_RUNTIME}/lib)
FILE(GLOB sources *.cpp)

ADD_EXECUTABLE(ProcessMonitor_test ${sources})
TARGET_LINK_LIBRARIES (ProcessMonitor_test ACE lluf_utils)

ADD_TEST(ProcessMonitor ProcessMonitor_test)
