PROJECT (dose_internal_test)

INCLUDE($ENV{SAFIR_SDK}/data/build/cmake_settings.txt)

ADD_EXECUTABLE(dose_connector_test dose_connector_test.cpp)
ADD_EXECUTABLE(dose_connectee_test dose_connectee_test.cpp)
ADD_EXECUTABLE(distribution_data_test distribution_data_test.cpp)
ADD_EXECUTABLE(lamport_clock_test lamport_clock_test.cpp)
ADD_EXECUTABLE(dose_message_queue_test dose_message_queue_test.cpp)

TARGET_LINK_LIBRARIES(dose_connector_test dose_internal dots_id dots_kernel dots_generated-cpp dots_cpp expat boost_crypto boost_filesystem boost_date_time lluf_utils)
TARGET_LINK_LIBRARIES(dose_connectee_test dose_internal dots_id dots_kernel dots_generated-cpp dots_cpp expat boost_crypto boost_filesystem boost_date_time lluf_utils)
TARGET_LINK_LIBRARIES(distribution_data_test dose_internal dots_id dots_kernel dots_generated-cpp dots_cpp expat boost_crypto boost_filesystem boost_date_time lluf_utils)
TARGET_LINK_LIBRARIES(lamport_clock_test dose_internal dots_id dots_kernel dots_generated-cpp dots_cpp expat boost_crypto boost_filesystem boost_date_time lluf_utils)
TARGET_LINK_LIBRARIES(dose_message_queue_test dose_internal dots_generated-cpp dots_cpp)

INSTALL(TARGETS dose_connectee_test dose_connector_test distribution_data_test lamport_clock_test dose_message_queue_test
        RUNTIME DESTINATION $ENV{SAFIR_RUNTIME}/bin
        LIBRARY DESTINATION $ENV{SAFIR_RUNTIME}/lib
        ARCHIVE DESTINATION $ENV{SAFIR_SDK}/lib)
