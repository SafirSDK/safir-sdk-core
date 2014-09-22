FILE(GLOB dod_files data/*.dod)

INSTALL(FILES
  ${dod_files}
  DESTINATION ${SAFIR_INSTALL_DESTINATION_GENERATION}/dod
  COMPONENT Development)

#TODO  data/dots_unit.xsd

INSTALL(FILES
          data/precompiled_header_for_cpp.h
          data/precompiled_header_for_cpp.cpp
#          data/dots_generated_ada.gpr_template
#          data/dots_generated_ada_library.gpr_template
#          data/dots_generated_main_ada.gpr_template
        DESTINATION ${SAFIR_INSTALL_DESTINATION_GENERATION}/cpp
        COMPONENT Development)

#TODO versioning of the dots_generated assemblies?

INSTALL(FILES
#          data/AssemblyInfo.cs TODO?
          data/dots_generated-dotnet.snk
        DESTINATION ${SAFIR_INSTALL_DESTINATION_GENERATION}/dotnet
        COMPONENT Development)

INSTALL(FILES
          data/Manifest.txt.in
        DESTINATION ${SAFIR_INSTALL_DESTINATION_GENERATION}/java
        COMPONENT Development)



#INSTALL(DIRECTORY
#          data/dobmake_config
#        DESTINATION ${SAFIR_SDK}/dots/dots_generated)

INSTALL(PROGRAMS
  dots_v.py
  DESTINATION ${SAFIR_INSTALL_DESTINATION_BIN}
  COMPONENT Development)

#TODO        data/dobmake.py
