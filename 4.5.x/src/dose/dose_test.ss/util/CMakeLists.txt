cmake_minimum_required(VERSION 2.8)

ADD_SUBDIRECTORY(dose_memory_stats)
ADD_SUBDIRECTORY(dose_com_control)
ADD_SUBDIRECTORY(dose_test_util)

if (SAFIR_JAVA_SUPPORT)
  ADD_SUBDIRECTORY(dose_test_util_java)
endif()
