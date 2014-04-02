#this library has an interface which is okay to call between debug/release runtimes,
#and we only want one version of it, so we never want the "d" suffix on debug builds,
#which is default on windows.
SET(CMAKE_DEBUG_POSTFIX "")

add_library(lluf_startup_synchronizer SHARED StartupSynchronizer.cpp Safir/Utilities/StartupSynchronizer.h)

TARGET_INCLUDE_DIRECTORIES(lluf_startup_synchronizer PUBLIC .)

FIND_PACKAGE(Boost COMPONENTS filesystem thread system)

TARGET_LINK_LIBRARIES(lluf_startup_synchronizer lluf_config ${Boost_LIBRARIES})

INSTALL(DIRECTORY Safir DESTINATION ${SAFIR_SDK}/include
        PATTERN ".svn" EXCLUDE
        PATTERN "*~" EXCLUDE)

INSTALL(TARGETS lluf_startup_synchronizer
        RUNTIME DESTINATION ${SAFIR_RUNTIME}/bin
        LIBRARY DESTINATION ${SAFIR_RUNTIME}/lib
        ARCHIVE DESTINATION ${SAFIR_SDK}/lib)

INSTALL_DEBUG_INFO(lluf_startup_synchronizer)
