#this library has a C interface and we only want one version of it,
#so we never want the "d" suffix on debug builds, which is default on windows.
SET(CMAKE_DEBUG_POSTFIX "")

ADD_LIBRARY(lluf_id SHARED lluf_id.cpp md5.c md5.h)

TARGET_LINK_LIBRARIES(lluf_id PRIVATE
  lluf_utils)

SAFIR_INSTALL(TARGETS lluf_id)
