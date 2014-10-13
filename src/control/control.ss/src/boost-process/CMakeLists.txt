cmake_minimum_required(VERSION 2.8)

include(ExternalProject)

set(DOWNLOAD_URL http://www.highscore.de/boost/process0.5/process.zip)

ExternalProject_Add(
    boost-process
    PREFIX ""
    SOURCE_DIR src
    URL ${DOWNLOAD_URL}
    CONFIGURE_COMMAND ""
    UPDATE_COMMAND ""
    PATCH_COMMAND ""
    BUILD_COMMAND ""
    INSTALL_COMMAND ""
    )

