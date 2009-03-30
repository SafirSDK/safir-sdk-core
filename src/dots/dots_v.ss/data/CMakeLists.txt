#Things that need to be done (TODO):
# build a shared library from ada files
# maybe make clean remove the directories
# more ?


PROJECT (dots_generated)

FILE(MAKE_DIRECTORY cpp/include)
#FILE(MAKE_DIRECTORY ada/lib-obj
#                    ada/lib-obj/release
#                    ada/lib-obj/debug 
#                    ada/lib-obj/release-lib
#                    ada/lib-obj/debug-lib)
#FILE(MAKE_DIRECTORY java/bin)

INCLUDE_DIRECTORIES(. cpp/include)
INCLUDE($ENV{SAFIR_SDK}/data/build/cmake_settings.txt)

FILE(GLOB dou_files *.dou)

SET(dod_directory $ENV{SAFIR_RUNTIME}/data/text/dots/config)
FILE(GLOB dod_files ${dod_directory} *.dod)

#Loop over all dou files and add rules to generate them
FOREACH(dou_file ${dou_files})
        STRING(REGEX REPLACE "/([a-zA-Z.0-9]*).dou" "/cpp/\\1.cpp" cpp_file ${dou_file})
	SET(cpp_files ${cpp_files} ${cpp_file})

        STRING(REGEX REPLACE "/([a-zA-Z.0-9]*).dou" "/dotnet/\\1.cs" cs_file ${dou_file})
	SET(cs_files ${cs_files} ${cs_file})

	STRING(REGEX REPLACE "/([a-zA-Z.0-9]*).dou" "/tags/\\1.txt" txt_file ${dou_file})
	STRING(REPLACE "." "-" txt_file ${txt_file})
	STRING(REPLACE "-txt" ".txt" txt_file ${txt_file})

#	STRING(REGEX REPLACE "/([a-zA-Z.0-9]*).dou" "/ada/\\1.adb" adb_file ${dou_file})
#	STRING(REPLACE "." "-" adb_file ${adb_file})
#	STRING(REPLACE "-adb" ".adb" adb_file ${adb_file})
#	STRING(TOLOWER ${adb_file} adb_file)
#	STRING(REPLACE ".adb" ".ads" ads_file ${adb_file})
#	SET(ada_files ${ada_files} ${ads_file} ${adb_file})

#	STRING(REGEX REPLACE "(.*)/([a-zA-Z.0-9]*).dou" "\\1" java_path ${dou_file})
#	STRING(REGEX REPLACE "(.*)/([a-zA-Z.0-9]*).dou" "/java/src/\\2.java" java_full ${dou_file})
#	STRING(REPLACE "." "/" java_full ${java_full})
#	STRING(REGEX REPLACE "/java$" ".java" java_full ${java_full})
#	STRING(REGEX REPLACE "/(.*)/[a-zA-Z0-9]*.java" "/\\1" java_dir ${java_full})
#	STRING(REGEX REPLACE "/.*/([a-zA-Z0-9]*).java" "\\1.java" java_name ${java_full})
#        STRING(TOLOWER ${java_dir} java_dir)
#	SET(java_file ${java_path}${java_dir}/${java_name})
#	SET(java_files ${java_files} ${java_file})


	STRING(REGEX REPLACE ".*/([a-zA-Z.0-9]*).dou" "\\1.dou" dou_file_relative ${dou_file})

#	ADD_CUSTOM_COMMAND(OUTPUT ${cpp_file} ${cs_file} ${txt_file} ${ads_file} ${adb_file} ${java_file}
	ADD_CUSTOM_COMMAND(OUTPUT ${cpp_file} ${cs_file} ${txt_file}
                           COMMAND dots_v ARGS -dod=${dod_directory} ${dou_file_relative}
                           DEPENDS ${dod_files} ${dou_file})
ENDFOREACH(dou_file)


# --- C++ --- #

ADD_LIBRARY(dots_generated-cpp SHARED ${cpp_files})

# --- C# --- #
SET(dots_generated $ENV{SAFIR_SDK}/dots/dots_generated)

ADD_CUSTOM_COMMAND(OUTPUT ${dots_generated}/dotnet/dots_generated-dotnet.dll
                  COMMAND gmcs -warn:4 -nowarn:0162 -r:Safir.Dob.Typesystem.dll -out:dots_generated-dotnet.dll -target:library ${cs_files}
                  DEPENDS ${cs_files}
                  WORKING_DIRECTORY dotnet
                  COMMENT "Building dots_generated-dotnet"
                  VERBATIM)

ADD_CUSTOM_TARGET(dots_generated-dotnet.dll ALL
				  DEPENDS ${dots_generated}/dotnet/dots_generated-dotnet.dll)

# --- Ada --- #
#SET(gprmake_arguments -q -j3 -XBUILD=release -XWarning_Level=Default)
#ADD_CUSTOM_COMMAND(OUTPUT ada/libdots_generated-ada.so
#                   COMMAND gnatmake ${gprmake_arguments} -P ../dots_generated_library.gpr
#                   DEPENDS ${ada_files}
#                   WORKING_DIRECTORY ada
#                   COMMENT "Building dots_generated-ada"
#                   VERBATIM)
#
#SET(gprmake_arguments_dbg -q -j3 -XBUILD=debug -XWarning_Level=Default)
#ADD_CUSTOM_COMMAND(OUTPUT ada/libdots_generated-adad.so
#                   COMMAND gnatmake ${gprmake_arguments_dbg} -P ../dots_generated_library.gpr
#                   DEPENDS ${ada_files}
#                   WORKING_DIRECTORY ada
#                   COMMENT "Building dots_generated-adad"
#                   VERBATIM)
#
#ADD_CUSTOM_TARGET(libdots_generated-ada.so ALL
#                  DEPENDS ada/libdots_generated-ada.so)
#
#ADD_CUSTOM_TARGET(libdots_generated-adad.so ALL
#                  DEPENDS ada/libdots_generated-adad.so)

# --- Java --- #
#SET(java_class_path "$ENV{SAFIR_RUNTIME}/bin/safir.dob.typesystem.jar")
#ADD_CUSTOM_COMMAND(OUTPUT java/dots_generated-java.jar
#                   COMMAND javac -cp ${java_class_path} -d java/bin ${java_files}
#                   COMMAND jar cvf java/dots_generated-java.jar -C java/bin  .
#                   DEPENDS ${java_files}
#                   COMMENT "Building dots_generated-java"
#                   VERBATIM)
#ADD_CUSTOM_TARGET(dots_generated-java.jar ALL
#                  DEPENDS java/dots_generated-java.jar)



# --- Installation rules --- #

INSTALL(CODE "FILE(REMOVE_RECURSE $ENV{SAFIR_RUNTIME}/data/text/dots/classes/)")

FILE(GLOB dom_files *.dom)

INSTALL(FILES ${dou_files} ${dom_files} 
        DESTINATION $ENV{SAFIR_RUNTIME}/data/text/dots/classes/)

INSTALL(DIRECTORY cpp/include DESTINATION $ENV{SAFIR_SDK}
        PATTERN ".svn" EXCLUDE
        PATTERN "*~" EXCLUDE)

INSTALL(TARGETS dots_generated-cpp
        RUNTIME DESTINATION $ENV{SAFIR_RUNTIME}/bin
        LIBRARY DESTINATION $ENV{SAFIR_RUNTIME}/lib
        ARCHIVE DESTINATION $ENV{SAFIR_SDK}/lib)

INSTALL(FILES ${dots_generated}/dotnet/dots_generated-dotnet.dll
        DESTINATION $ENV{SAFIR_RUNTIME}/lib)

#INSTALL(FILES ada/lib-obj/release-lib/libdots_generated-ada.so
#        DESTINATION $ENV{SAFIR_RUNTIME}/lib/)

#INSTALL(FILES ada/lib-obj/debug-lib/libdots_generated-adad.so
#        DESTINATION $ENV{SAFIR_RUNTIME}/lib/)

#INSTALL(DIRECTORY ada/
#        DESTINATION $ENV{SAFIR_SDK}/ada/dots_generated-ada/
#        PATTERN "lib-obj" EXCLUDE
#        PATTERN "*.adb" EXCLUDE)

#INSTALL(DIRECTORY ada/lib-obj/release-lib/
#        DESTINATION $ENV{SAFIR_SDK}/ada/dots_generated-ada/lib-obj/release/
#        PATTERN "*.so" EXCLUDE)

#INSTALL(DIRECTORY ada/lib-obj/debug-lib/
#        DESTINATION $ENV{SAFIR_SDK}/ada/dots_generated-ada/lib-obj/debug/
#        PATTERN "*.so" EXCLUDE)

#INSTALL(FILES java/dots_generated-java.jar
#        DESTINATION $ENV{SAFIR_RUNTIME}/bin)
