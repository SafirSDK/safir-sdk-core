INCLUDE_DIRECTORIES(.)

FILE(GLOB_RECURSE headers *.h)
FILE(GLOB sources *.cpp)

ADD_EXECUTABLE(dots_depends ${sources} ${headers})

FIND_PACKAGE(Boost COMPONENTS filesystem system)
if (NOT MSVC)
  TARGET_LINK_LIBRARIES(dots_depends ${Boost_LIBRARIES})
endif()

INSTALL(TARGETS dots_depends
        RUNTIME DESTINATION ${SAFIR_RUNTIME}/bin
        )

INSTALL_DEBUG_INFO(dots_depends)
