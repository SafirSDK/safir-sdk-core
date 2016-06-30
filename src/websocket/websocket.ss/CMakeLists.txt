add_definitions(-D_WEBSOCKETPP_NO_CPP11_CHRONO_)

#for old visual studio versions we can't use some C++11 features of websocketpp
if (MSVC_VERSION LESS 1800)
  add_definitions(-D_WEBSOCKETPP_NO_CPP11_SYSTEM_ERROR_)
  add_definitions(-D_WEBSOCKETPP_NO_CPP11_FUNCTIONAL_)
endif()

include_directories(
  ${CMAKE_CURRENT_SOURCE_DIR}/dependencies/rapidjson/include
  ${CMAKE_CURRENT_SOURCE_DIR}/dependencies/websocketpp)

add_subdirectory(src)
add_subdirectory(test)
