#the deprecated feature still needs to build without warnings...
add_definitions(-DSAFIR_DISABLE_OLIB_DEPRECATION_WARNING)

ADD_LIBRARY(olib SHARED Connection.cpp  Diagnostics.cpp  Diagnostics.h  Environment.cpp  Statement.cpp  StringConversion.h)

TARGET_LINK_LIBRARIES(olib
  PRIVATE
  logging_cpp

  PUBLIC
  safir_generated-Core-cpp
  ${SAFIR_ODBC_LIBRARIES})

SAFIR_INSTALL(TARGETS olib)
