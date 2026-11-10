from conan import ConanFile
from conan.errors import ConanException
import subprocess


class QtSystemWrapper(ConanFile):
    name = "qt"

    # These options carry no meaning for this wrapper - nothing here reads them,
    # and the package stays empty whatever they are set to. They exist because
    # consumers inspect the real ConanCenter qt recipe's options, and asking for
    # an option a recipe does not declare is a hard error in Conan, not None.
    # qt-advanced-docking-system's validate() reads exactly these three
    # (gui/widgets directly, with_x11 on Linux and FreeBSD); its recipe revision
    # f45fe2b0766a4a30dff2aca7d3283e61, published 2026-11-04, added that check
    # and broke every Linux build until they were declared here. The defaults
    # describe what a distro Qt6 always provides, so the check passes. If a
    # consumer ever asks for a fourth option, add it the same way.
    options = {
        "gui": [True, False],
        "widgets": [True, False],
        "with_x11": [True, False],
    }
    default_options = {
        "gui": True,
        "widgets": True,
        "with_x11": True,
    }

    def set_version(self):
        result = subprocess.run(["pkg-config", "--modversion", "Qt6Core"],
                                capture_output=True, text=True)
        if result.returncode == 0:
            self.version = result.stdout.strip()
        else:
            raise ConanException("Could not detect system Qt6 version via pkg-config")

    def package_info(self):
        # Clear all default dirs so no paths are injected into the qt::qt target.
        # This satisfies qt-advanced-docking-system's Conan dependency chain without
        # providing any Qt content. System Qt is found separately in CMakeLists.txt
        # via find_package(Qt6 COMPONENTS ...), which falls through to the system
        # because CMakeConfigDeps generates qtConfig.cmake (lowercase package name),
        # not Qt6Config.cmake, so it never intercepts find_package(Qt6).
        #
        # The dummy define is required because CMakeConfigDeps only generates the
        # qt::qt INTERFACE target when cpp_info has some content; without it no
        # target is created and the qt6advanceddocking cmake files fail to resolve
        # their qt::qt dependency.
        self.cpp_info.defines = ["CONAN_QT_SYSTEM_PLACEHOLDER"]
        self.cpp_info.includedirs = []
        self.cpp_info.libdirs = []
        self.cpp_info.bindirs = []
        self.cpp_info.resdirs = []
