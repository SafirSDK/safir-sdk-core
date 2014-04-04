FIND_PACKAGE(Boost COMPONENTS thread system)

FIND_PACKAGE(PythonInterp)

ADD_SUBDIRECTORY(processes)
ADD_SUBDIRECTORY(threads)
ADD_SUBDIRECTORY(processes_and_threads)



