#this library has a C interface and we only want one version of it,
#so we never want the "d" suffix on debug builds, which is default on windows.
SET(CMAKE_DEBUG_POSTFIX "")

ADD_LIBRARY(lluf_id SHARED lluf_id.cpp md5.c md5.h Safir/Utilities/Internal/Id.h)

target_include_directories(lluf_id PUBLIC .)

FIND_PACKAGE(Boost COMPONENTS filesystem thread)

TARGET_LINK_LIBRARIES(lluf_id lluf_utils ${Boost_LIBRARIES})

INSTALL(DIRECTORY Safir DESTINATION ${SAFIR_SDK}/include
        PATTERN ".svn" EXCLUDE
        PATTERN "*~" EXCLUDE)

INSTALL(TARGETS lluf_id
        RUNTIME DESTINATION ${SAFIR_RUNTIME}/bin
        LIBRARY DESTINATION ${SAFIR_RUNTIME}/lib
        ARCHIVE DESTINATION ${SAFIR_SDK}/lib)

INSTALL(FILES ../data/lluf_id.gpr
        DESTINATION ${SAFIR_SDK}/ada/)

INSTALL_DEBUG_INFO(lluf_id)
