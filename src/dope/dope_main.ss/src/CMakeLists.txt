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
)

ADD_EXECUTABLE(dope_bin2xml dope_bin2xml.cpp)


TARGET_LINK_LIBRARIES(dope_main PRIVATE
  swre_interface_cpp
  olib
  dose_cpp
  logging_cpp
  ${Boost_ATOMIC_LIBRARY}
  ${Boost_FILESYSTEM_LIBRARY})

TARGET_LINK_LIBRARIES(dope_bin2xml PRIVATE
  olib
  safir_generated-Core-cpp
  ${Boost_FILESYSTEM_LIBRARY}
  ${Boost_PROGRAM_OPTIONS_LIBRARY})

SAFIR_INSTALL(TARGETS dope_main dope_bin2xml)
