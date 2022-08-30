#this library has a C interface and we only want one version of it,
#so we never want the "d" suffix on debug builds, which is default on windows.
SET(CMAKE_DEBUG_POSTFIX "")

SET(sources 
dots_exception_keeper.h
dots_init_helper.h
dots_repository_keeper.h
dots_shm_repository.h
dots_exception_keeper.cpp
dots_kernel.cpp
dots_repository_keeper.cpp
dots_shm_repository.cpp)

ADD_LIBRARY(dots_kernel SHARED ${sources})

#Make our INTERNAL headers available to other parts of Core.
target_include_directories(dots_kernel PUBLIC $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}/include>)

TARGET_LINK_LIBRARIES(dots_kernel PRIVATE
  dots_internal
  lluf_internal
  lluf_startup_synchronizer)

if (UNIX)
  TARGET_LINK_LIBRARIES(dots_kernel PRIVATE rt)
endif()

SAFIR_INSTALL(TARGETS dots_kernel)

