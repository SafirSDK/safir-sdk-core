FILE(GLOB dod_files data/*.dod)

#TODO: install dod files and other needed things

INSTALL(FILES 
  ${dod_files} 
  DESTINATION ${SAFIR_INSTALL_DESTINATION_GENERATION}/dod)

#TODO  data/dots_unit.xsd

INSTALL(FILES 
#          data/AssemblyInfo.cs
#          data/dots_generated-dotnet.snk
#          data/CMakeLists.txt 
#          data/Manifest.java.txt
          data/precompiled_header_for_cpp.h
#          data/precompiled_header_for_cpp.cpp
#          data/dots_generated_ada.gpr_template
#          data/dots_generated_ada_library.gpr_template
#          data/dots_generated_main_ada.gpr_template
        DESTINATION ${SAFIR_INSTALL_DESTINATION_GENERATION}/cpp)

INSTALL(FILES 
#          data/AssemblyInfo.cs TODO?
          data/dots_generated-dotnet.snk
        DESTINATION ${SAFIR_INSTALL_DESTINATION_GENERATION}/dotnet)


#TODO need better spot for these, and what about .cpp?
# INSTALL(FILES 
#           data/precompiled_header_for_cpp.h
#         DESTINATION ${SAFIR_SDK}/include)

#INSTALL(DIRECTORY
#          data/dobmake_config
#        DESTINATION ${SAFIR_SDK}/dots/dots_generated)

#INSTALL(FILES
#        data/gen/CMakeLists.txt
#        DESTINATION ${SAFIR_SDK}/dots/dots_generated/gen)
     
INSTALL(PROGRAMS
  dots_v.py
  DESTINATION ${SAFIR_INSTALL_DESTINATION_BIN})

# #        data/dobmake.py
