PROJECT (TestDoseCpp)

INCLUDE_DIRECTORIES(.)

INCLUDE($ENV{SAFIR_SDK}/data/build/cmake_settings.txt)

ADD_EXECUTABLE(TestConnection TestConnection.cpp)
ADD_EXECUTABLE(TestSendMsg TestSendMsg.cpp)
ADD_EXECUTABLE(TestReceiveMsg TestReceiveMsg.cpp)
ADD_EXECUTABLE(TestRegisterer TestRegisterer.cpp)
ADD_EXECUTABLE(TestRegistrationSubscriber TestRegistrationSubscriber.cpp)
#ADD_EXECUTABLE(TestEntityOwner TestEntityOwner.cpp)
#ADD_EXECUTABLE(TestEntitySubscriber TestEntitySubscriber.cpp)

TARGET_LINK_LIBRARIES(TestConnection dose_cpp dose_dll dose_internal dots_id dots_kernel dots_generated-cpp dots_cpp ACE boost_regex)
TARGET_LINK_LIBRARIES(TestSendMsg dose_cpp dose_dll dose_internal dots_id dots_kernel dots_generated-cpp dots_cpp ACE boost_regex)
TARGET_LINK_LIBRARIES(TestReceiveMsg dose_cpp dose_dll dose_internal dots_id dots_kernel dots_generated-cpp dots_cpp ACE boost_regex)
TARGET_LINK_LIBRARIES(TestRegisterer dose_cpp dose_dll dose_internal dots_id dots_kernel dots_generated-cpp dots_cpp ACE boost_regex)
TARGET_LINK_LIBRARIES(TestRegistrationSubscriber dose_cpp dose_dll dose_internal dots_id dots_kernel dots_generated-cpp dots_cpp ACE boost_regex)
#TARGET_LINK_LIBRARIES(TestEntityOwner dose_cpp dose_dll dose_internal dots_id dots_kernel dots_generated-cpp dots_cpp ACE boost_regex)
#TARGET_LINK_LIBRARIES(TestEntitySubscriber dose_cpp dose_dll dose_internal dots_id dots_kernel dots_generated-cpp dots_cpp ACE boost_regex)

