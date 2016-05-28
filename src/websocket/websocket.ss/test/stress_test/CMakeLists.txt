file(GLOB headers *.h)
file(GLOB sources *.cpp)

add_executable(safir_websocket_stresstest
                ${sources} ${headers})

target_link_libraries(safir_websocket_stresstest  PRIVATE
    ${Boost_PROGRAM_OPTIONS_LIBRARY}
    ${Boost_RANDOM_LIBRARY}
    ${Boost_THREAD_LIBRARY}
    ${Boost_SYSTEM_LIBRARY})

#add_test(NAME WebsocketUnitTests COMMAND safir_websocket_unittests)
#SET_SAFIR_TEST_PROPERTIES(TEST WebsocketUnitTests TIMEOUT 360)
