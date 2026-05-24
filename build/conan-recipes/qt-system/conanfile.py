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
        # Clear all default dirs so CMakeDeps generates a completely empty qt::qt
        # INTERFACE target. This satisfies qt-advanced-docking-system's Conan dependency
        # chain without injecting any paths. System Qt is found separately in CMakeLists.txt
        # via find_package(Qt6 COMPONENTS ...), which falls through to the system because
        # there is no Qt6Config.cmake in the Conan generators folder.
        self.cpp_info.includedirs = []
        self.cpp_info.libdirs = []
        self.cpp_info.bindirs = []
        self.cpp_info.resdirs = []
