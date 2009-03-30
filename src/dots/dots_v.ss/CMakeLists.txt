PROJECT (dots_v)

cmake_minimum_required(VERSION 2.4)

#This whole block is to make linux with xmlada3 use the correct code
#rather "hackish" solution, but it appears to work, for now.
SET(xmlada3_path "xmlada3_path-NOTFOUND" CACHE FILEPATH "Cleared." FORCE)
FIND_FILE(xmlada3_path "libxmlada.so.3" "/usr/lib" "/usr/local/lib")
SET(USE_XMLADA3 false)
if (NOT xmlada3_path STREQUAL xmlada3_path-NOTFOUND)
  SET(USE_XMLADA3 true)
endif (NOT xmlada3_path STREQUAL xmlada3_path-NOTFOUND)

SET(gprmake_arguments -q -XBUILD=release -XWarning_Level=Default -XUSE_XMLADA3=${USE_XMLADA3})

SET(build_directories program/lib-obj/release 
		      program/lib-obj/debug 
                      lib-obj/release 
		      lib-obj/debug)

FILE(MAKE_DIRECTORY ${build_directories})

#FILE(GLOB gpr_files data/*.gpr)
#FOREACH(gpr_file ${gpr_files})
#    CONFIGURE_FILE(${gpr_file} $ENV{SAFIR_SDK}/lib/projects/ COPYONLY)
#ENDFOREACH(gpr_file)

ADD_CUSTOM_TARGET(dots_v ALL
                  COMMAND gnatmake ${gprmake_arguments} -P dots_v-program.gpr 
                  WORKING_DIRECTORY program
                  COMMENT "Building dots_v using gprmake"
                  VERBATIM)

#INSTALL(FILES data/dots_generated_ada.gpr
#        DESTINATION $ENV{SAFIR_SDK}/ada)


INSTALL(PROGRAMS program/dots_v
        DESTINATION $ENV{SAFIR_RUNTIME}/bin)

FILE(GLOB dod_files data/*.dod)
INSTALL(FILES ${dod_files}
              data/dots_unit.xsd
        DESTINATION $ENV{SAFIR_RUNTIME}/data/text/dots/config/)

INSTALL(FILES data/CMakeLists.txt 
              data/precompiled_header_for_cpp.h
        DESTINATION $ENV{SAFIR_SDK}/dots/dots_generated)
