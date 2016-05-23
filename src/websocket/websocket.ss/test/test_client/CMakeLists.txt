file(GLOB headers *.h)
file(GLOB sources *.cpp)

add_definitions(-D_WEBSOCKETPP_NO_CPP11_CHRONO_)
include_directories(${CMAKE_CURRENT_SOURCE_DIR}/../../include)

add_executable(safir_websocket_testclient 
                ${sources} ${headers}) 

target_link_libraries(safir_websocket_testclient  PRIVATE
    ${Boost_PROGRAM_OPTIONS_LIBRARY}
    ${Boost_RANDOM_LIBRARY}
    ${Boost_THREAD_LIBRARY}
    ${Boost_SYSTEM_LIBRARY})

#add_test(NAME WebsocketUnitTests COMMAND safir_websocket_unittests)
#SET_SAFIR_TEST_PROPERTIES(TEST WebsocketUnitTests TIMEOUT 360)
