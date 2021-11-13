INCLUDE_DIRECTORIES(../../src/include)

ADD_EXECUTABLE(IpcPublisher ipc_publisher.cpp)
ADD_EXECUTABLE(IpcSubscriber ipc_subscriber.cpp)

TARGET_LINK_LIBRARIES(IpcPublisher PRIVATE
  lluf_utils
  Boost::program_options)

TARGET_LINK_LIBRARIES(IpcSubscriber PRIVATE
  lluf_utils
  Boost::program_options)

ADD_TEST(NAME Ipc COMMAND ${Python3_EXECUTABLE} ${CMAKE_CURRENT_SOURCE_DIR}/run_test.py)
SET_SAFIR_TEST_PROPERTIES(TEST Ipc TIMEOUT 240)
