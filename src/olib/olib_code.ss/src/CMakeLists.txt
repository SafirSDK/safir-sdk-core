ADD_LIBRARY(olib SHARED Connection.cpp  Diagnostics.cpp  Diagnostics.h  Environment.cpp  Statement.cpp  StringConversion.h)

TARGET_LINK_LIBRARIES(olib PRIVATE 
#  douf_time_cpp 
  dots_generated-olib-cpp
#  logging_cpp
  ${SAFIR_ODBC_LIBRARIES})

SAFIR_INSTALL(TARGETS olib)
