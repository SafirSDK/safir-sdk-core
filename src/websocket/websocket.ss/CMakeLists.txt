find_package(RapidJSON REQUIRED)
find_package(websocketpp REQUIRED)

add_definitions(-D_WEBSOCKETPP_NO_CPP11_CHRONO_)

add_subdirectory(src)
add_subdirectory(test)
