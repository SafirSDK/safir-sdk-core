FILE(GLOB dod_files data/*.dod)

INSTALL(FILES
  ${dod_files}
  DESTINATION ${SAFIR_INSTALL_DESTINATION_GENERATION}/dod
  COMPONENT Development)

INSTALL(FILES
          data/precompiled_header_for_cpp.h
        DESTINATION ${SAFIR_INSTALL_DESTINATION_GENERATION}/cpp
        COMPONENT Development)

INSTALL(FILES
          data/dots_generated-dotnet.snk
        DESTINATION ${SAFIR_INSTALL_DESTINATION_GENERATION}/dotnet
        COMPONENT Development)

INSTALL(FILES
          data/Manifest.txt.in
        DESTINATION ${SAFIR_INSTALL_DESTINATION_GENERATION}/java
        COMPONENT Development)

SAFIR_INSTALL_PYTHON(SCRIPT dots_v.py
  COMPONENT Development)

