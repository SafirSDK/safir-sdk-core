ADD_SUBDIRECTORY(boost-process)

INCLUDE_DIRECTORIES(${CMAKE_CURRENT_BINARY_DIR}/boost-process/src)

FILE(GLOB headers *.h)
FILE(GLOB sources *.cpp)

ADD_EXECUTABLE(safir_control ${sources} ${headers})

TARGET_LINK_LIBRARIES(safir_control
  system_picture
  control_utils)

add_dependencies(safir_control boost-process)

TARGET_LINK_LIBRARIES(safir_control
  ${Boost_FILESYSTEM_LIBRARY}
  ${Boost_PROGRAM_OPTIONS_LIBRARY})

SAFIR_INSTALL(TARGETS safir_control)
