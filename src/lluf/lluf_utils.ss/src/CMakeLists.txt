

FILE(GLOB_RECURSE headers *.h)
FILE(GLOB sources *.cpp)

ADD_LIBRARY(lluf_utils SHARED ${sources} ${headers})

target_include_directories(lluf_utils PUBLIC .)

FIND_PACKAGE(Boost COMPONENTS filesystem system thread program_options)

if (NOT MSVC)
  TARGET_LINK_LIBRARIES(lluf_utils lluf_startup_synchronizer lluf_config ${Boost_LIBRARIES} dl rt)
endif()

#on windows we need the psapi library
if (WIN32)
  TARGET_LINK_LIBRARIES(lluf_utils psapi)
endif(WIN32)

INSTALL(DIRECTORY Safir DESTINATION ${SAFIR_SDK}/include
        PATTERN ".svn" EXCLUDE
        PATTERN "*~" EXCLUDE)

INSTALL(TARGETS lluf_utils 
        RUNTIME DESTINATION ${SAFIR_RUNTIME}/bin
        LIBRARY DESTINATION ${SAFIR_RUNTIME}/lib
        ARCHIVE DESTINATION ${SAFIR_SDK}/lib)


INSTALL_DEBUG_INFO(lluf_utils)

if (WIN32)
INSTALL(FILES ../data/safir_event_log_registration.reg
        DESTINATION ${SAFIR_RUNTIME}/data/text/)
endif()
