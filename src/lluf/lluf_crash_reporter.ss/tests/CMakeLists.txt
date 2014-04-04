FIND_PACKAGE(Boost COMPONENTS thread system)

FIND_PACKAGE(PythonInterp)

ADD_EXECUTABLE(start_stop start_stop.cpp)
TARGET_LINK_LIBRARIES (start_stop lluf_crash_reporter)

ADD_TEST(CrashReporter_start_stop start_stop)
SET_SAFIR_TEST_PROPERTIES(CrashReporter_start_stop)

ADD_SUBDIRECTORY(simple_crash_test)
ADD_SUBDIRECTORY(external_signals_test)
ADD_SUBDIRECTORY(immediate_dump_test)


