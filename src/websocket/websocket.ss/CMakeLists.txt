find_package(RapidJSON)
find_package(websocketpp)

add_definitions(-D_WEBSOCKETPP_NO_CPP11_CHRONO_)

add_subdirectory(src)
add_subdirectory(test)
