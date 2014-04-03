ADD_EXECUTABLE(IpcPublisher ipc_publisher.cpp)
ADD_EXECUTABLE(IpcSubscriber ipc_subscriber.cpp)

IF (NOT MSVC)
   TARGET_LINK_LIBRARIES (IpcPublisher lluf_config lluf_utils ${Boost_LIBRARIES})
   TARGET_LINK_LIBRARIES (IpcSubscriber lluf_config lluf_utils ${Boost_LIBRARIES})
ENDIF()

ADD_TEST(Ipc ${PYTHON_EXECUTABLE} ${CMAKE_CURRENT_SOURCE_DIR}/run_test.py)
SET_SAFIR_TEST_PROPERTIES(Ipc 240)
