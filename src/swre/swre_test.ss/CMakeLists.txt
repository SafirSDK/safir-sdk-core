cmake_minimum_required(VERSION 2.8)

project(swre_test CXX)

include($ENV{SAFIR_SDK}/data/build/cmake_settings.txt)

find_package(Boost COMPONENTS thread date_time system)

find_package(PythonInterp)

FIND_SOURCE_ROOT(SOURCE_ROOT)

enable_testing()

add_subdirectory(panic_log_reception)
add_subdirectory(swreport_reception)
add_subdirectory(tracer_cpp)
add_subdirectory(tracer_dotnet)
add_subdirectory(stop_orders_at_exit)
