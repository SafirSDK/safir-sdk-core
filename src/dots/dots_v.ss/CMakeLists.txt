FILE(GLOB dod_files data/*.dod)

# INSTALL(FILES 
#           ${dod_files} 
# #          data/dots_unit.xsd
#         DESTINATION ${SAFIR_RUNTIME}/data/text/dots/config/)

#INSTALL(FILES 
#          data/AssemblyInfo.cs
#          data/dots_generated-dotnet.snk
#          data/CMakeLists.txt 
#          data/Manifest.java.txt
#          data/precompiled_header_for_cpp.h
#          data/precompiled_header_for_cpp.cpp
#          data/dots_generated_ada.gpr_template
#          data/dots_generated_ada_library.gpr_template
#          data/dots_generated_main_ada.gpr_template
#        DESTINATION ${SAFIR_SDK}/dots/dots_generated)

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
     
# INSTALL(PROGRAMS
#         dots_v.py
# #        data/dobmake.py
#         DESTINATION ${SAFIR_RUNTIME}/bin)
