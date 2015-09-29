ADD_EXECUTABLE(dope_main
  DopeApp.h
  dope_main.cpp
  OdbcPersistor.cpp
  PersistenceHandler.h
  Defs.h
  FilePersistor.cpp
  OdbcPersistor.h
  DopeApp.cpp
  FilePersistor.h
  PersistenceHandler.cpp
  OdbcHelper.h
  OdbcHelper.cpp)

ADD_EXECUTABLE(dope_bin2xml dope_bin2xml.cpp OdbcHelper.h OdbcHelper.cpp)

ADD_EXECUTABLE(run_sql run_sql.cpp OdbcHelper.h OdbcHelper.cpp)

TARGET_LINK_LIBRARIES(dope_main PRIVATE
  swre_interface_cpp
  dose_cpp
  logging_cpp
  ${Boost_ATOMIC_LIBRARY}
  ${Boost_FILESYSTEM_LIBRARY}
  ${SAFIR_ODBC_LIBRARIES})

TARGET_LINK_LIBRARIES(dope_bin2xml PRIVATE
  safir_generated-Core-cpp
  ${Boost_FILESYSTEM_LIBRARY}
  ${Boost_PROGRAM_OPTIONS_LIBRARY}
  ${SAFIR_ODBC_LIBRARIES})

TARGET_LINK_LIBRARIES(run_sql PRIVATE
  ${Boost_PROGRAM_OPTIONS_LIBRARY}
  ${SAFIR_ODBC_LIBRARIES})

SAFIR_INSTALL(TARGETS dope_main dope_bin2xml)

SAFIR_INSTALL(TARGETS run_sql TEST_SUITE)

