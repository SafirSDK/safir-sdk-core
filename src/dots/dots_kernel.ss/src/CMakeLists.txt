#this library has a C interface and we only want one version of it,
#so we never want the "d" suffix on debug builds, which is default on windows.
SET(CMAKE_DEBUG_POSTFIX "")

FILE(GLOB_RECURSE headers *.h)
FILE(GLOB sources *.cpp)

ADD_LIBRARY(dots_kernel SHARED ${sources} ${headers})

target_include_directories(dots_kernel PUBLIC .)

FIND_PACKAGE(Boost COMPONENTS regex filesystem system)


TARGET_LINK_LIBRARIES(dots_kernel dots_internal lluf_utils lluf_startup_synchronizer lluf_crash_reporter lluf_config ${Boost_LIBRARIES})

#check if we can do unaligned accesses, or if we should define a symbol that fixes it.
include(CheckCXXSourceRuns)
SET(CMAKE_REQUIRED_FLAGS ${CMAKE_REQUIRED_FLAGS} -O2)
CHECK_CXX_SOURCE_RUNS("#include \"${CMAKE_CURRENT_SOURCE_DIR}/checks/check_unaligned_access.cpp\"" can_do_unaligned_access)
if(NOT can_do_unaligned_access)
  MESSAGE("This platform does not appear able to do unaligned accesses, so we'll use memcpy instead")
  ADD_DEFINITIONS(-DNO_UNALIGNED_ACCESS)
endif()


INSTALL(DIRECTORY Safir DESTINATION ${SAFIR_SDK}/include
        PATTERN ".svn" EXCLUDE
        PATTERN "*~" EXCLUDE)

INSTALL(TARGETS dots_kernel
        RUNTIME DESTINATION ${SAFIR_RUNTIME}/bin
        LIBRARY DESTINATION ${SAFIR_RUNTIME}/lib
        ARCHIVE DESTINATION ${SAFIR_SDK}/lib)

INSTALL(FILES ../data/dots_kernel.gpr
        DESTINATION ${SAFIR_SDK}/ada/)

INSTALL_DEBUG_INFO(dots_kernel)
