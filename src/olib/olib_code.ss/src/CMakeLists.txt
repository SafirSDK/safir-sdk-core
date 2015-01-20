ADD_LIBRARY(olib SHARED Connection.cpp  Diagnostics.cpp  Diagnostics.h  Environment.cpp  Statement.cpp  StringConversion.h)

TARGET_LINK_LIBRARIES(olib
  PRIVATE
  logging_cpp

  PUBLIC
  safir_generated-Core-cpp
  ${SAFIR_ODBC_LIBRARIES})

SAFIR_INSTALL(TARGETS olib)
