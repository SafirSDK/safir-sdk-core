ADD_LIBRARY(lluf_id SHARED lluf_id.cpp md5.c md5.h)
safir_mark_single_abi(lluf_id)

TARGET_LINK_LIBRARIES(lluf_id PRIVATE
  lluf_utils)

SAFIR_INSTALL(TARGETS lluf_id)
