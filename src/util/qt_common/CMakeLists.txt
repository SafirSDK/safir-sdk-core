add_library(qt_common INTERFACE)
target_include_directories(qt_common INTERFACE ${CMAKE_CURRENT_SOURCE_DIR})
set(QT_STYLESHEET_DIR ${CMAKE_CURRENT_SOURCE_DIR}/style CACHE INTERNAL "Path to shared Qt stylesheets")
