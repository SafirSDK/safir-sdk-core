from conan import ConanFile
from conan.errors import ConanException
import subprocess


class QtSystemWrapper(ConanFile):
    name = "qt"

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
