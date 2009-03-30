PROJECT (dose_com_monitor_java)

FILE(GLOB java_files *.java)
FILE(REMOVE_RECURSE bin)
FILE(MAKE_DIRECTORY bin)

ADD_CUSTOM_COMMAND(OUTPUT dose_com_monitor.jar
                   COMMAND javac ${java_files} -d bin/
                   COMMAND jar cfm dose_com_monitor.jar manifest.txt -C bin .
				   DEPENDS ${java_files}
                   COMMENT "Building dose_com_monitor.jar"
                   VERBATIM)

ADD_CUSTOM_TARGET(dose_com_monitor ALL
				  DEPENDS dose_com_monitor.jar)

INSTALL(FILES dose_com_monitor.jar
        DESTINATION $ENV{SAFIR_RUNTIME}/bin)
