#this library has a C interface and we only want one version of it,
#so we never want the "d" suffix on debug builds, which is default on windows.
SET(CMAKE_DEBUG_POSTFIX "")

ADD_LIBRARY(lluf_id SHARED lluf_id.cpp md5.c md5.h Safir/Utilities/Internal/Id.h)

target_include_directories(lluf_id PUBLIC .)

TARGET_LINK_LIBRARIES(lluf_id PRIVATE 
  lluf_utils
  ${Boost_THREAD_LIBRARY}
  ${Boost_SYSTEM_LIBRARY})

SAFIR_INSTALL(TARGETS lluf_id INCLUDE_DIR Safir)

# INSTALL(FILES ../data/lluf_id.gpr
#         DESTINATION ${SAFIR_SDK}/ada/)
