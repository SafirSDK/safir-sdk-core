include(ExternalProject)

#update these two values when a new boost process is released.
set(DOWNLOAD_URL http://www.highscore.de/boost/process0.5/process.zip)
set(URL_MD5 aa7e995dddf8a2cbb1c581b344f44b6e)

ExternalProject_Add(
    boost-process
    PREFIX ""
    SOURCE_DIR src
    URL ${DOWNLOAD_URL}
    URL_MD5 ${URL_MD5}
    CONFIGURE_COMMAND ""
    UPDATE_COMMAND ""
    PATCH_COMMAND ""
    BUILD_COMMAND ""
    INSTALL_COMMAND ""
    )

