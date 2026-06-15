""" Conan stuff for Safir SDK Core """
from conan import ConanFile
from conan.tools.cmake import CMakeConfigDeps
from conan.tools.files import copy
import os

class SafirSdkCoreConan(ConanFile):
    """ Conan stuff for Safir SDK Core """
    settings = "os", "compiler", "build_type", "arch"
    default_options = {"protobuf/*:lite":True,
                       "protobuf/*:shared":False,
                       "protobuf/*:with_zlib":False,

                       # All the options below are only used on Windows
                       "boost/*:shared": False,
                       "boost/*:layout":"versioned",
                       "boost/*:error_code_header_only":True,
                       "boost/*:system_no_deprecated":True,
                       "boost/*:filesystem_no_deprecated":True,
                       "boost/*:filesystem_version":3,
                       "boost/*:magic_autolink":False,
                       "boost/*:without_wave": True,
                       "boost/*:without_fiber": True,
                       "boost/*:without_contract": True,
                       "boost/*:without_coroutine": True,
                       "boost/*:without_serialization": True,
                       "boost/*:without_stacktrace": True,
                       "boost/*:without_python": True,
                       "boost/*:without_graph": True,
                       "boost/*:without_graph_parallel": True,
                       "boost/*:without_math": True,
                       "boost/*:without_mpi": True,
                       "boost/*:without_json": True,
                       "boost/*:without_type_erasure": True,
                       "boost/*:without_url": True,
                       "boost/*:bzip2": False,
                       "boost/*:zlib": False,

                       "qt/*:shared": False,
                       "qt/*:opengl": "no",
                       "qt/*:openssl": False,
                       "qt/*:with_vulkan": False,
                       "qt/*:with_pcre2": True,
                       "qt/*:with_glib": False,
                       "qt/*:with_doubleconversion": True,
                       "qt/*:with_freetype": False,
                       "qt/*:with_harfbuzz": False,
                       "qt/*:with_libjpeg": False,
                       "qt/*:with_libpng": True,
                       "qt/*:with_sqlite3": False,
                       "qt/*:with_mysql": False,
                       "qt/*:with_pq": False,
                       "qt/*:with_odbc": False,
                       "qt/*:with_openal": False,
                       "qt/*:with_zstd": False,
                       "qt/*:with_gstreamer": False,
                       "qt/*:with_pulseaudio": False,
                       "qt/*:with_dbus": False,
                       "qt/*:qtactiveqt": False,
                       "qt/*:qtcharts": False,
                       "qt/*:qtconnectivity": False,
                       "qt/*:qtdatavis3d": False,
                       "qt/*:qtimageformats": False,
                       "qt/*:qtlottie": False,
                       "qt/6*:qtmultimedia": False,
                       "qt/*:qtnetworkauth": False,
                       "qt/*:qtquick3d": False,
                       "qt/*:qtquicktimeline": False,
                       "qt/*:qtremoteobjects": False,
                       "qt/*:qtscxml": False,
                       "qt/*:qtsensors": False,
                       "qt/*:qtserialbus": False,
                       "qt/*:qtserialport": False,
                       "qt/*:qtsvg": True,
                       "qt/*:qtvirtualkeyboard": False,
                       "qt/*:qtwebchannel": False,
                       "qt/*:qtwebengine": False,
                       "qt/*:qtwebsockets": True,
                       "qt/*:qtwebview": False,
                       "qt/*:gui": True,
                       "qt/*:widgets": True,
                       "pcre2/*:shared": False,
                       "pcre2/*:with_zlib":False,
                       "pcre2/*:with_bzip2": False,
                       #"cmake/*:with_openssl": False,
                       #"cmake/*:bootstrap": True
                       }
    def generate(self):
        deps = CMakeConfigDeps(self)
        # When conan installs Release packages on behalf of a RelWithDebInfo cmake
        # build (to reuse ConanCenter pre-built binaries), CMakeConfigDeps must
        # generate files labelled RelWithDebInfo so cmake can find them. The cmake
        # build signals this via user.safir:cmake_build_type.
        cmake_build_type = self.conf.get("user.safir:cmake_build_type", default=None)
        if cmake_build_type:
            deps.configuration = cmake_build_type
        deps.generate()

        for dep in self.dependencies.values():
            # Skip platform dependencies (they don't have package_folder)
            if not dep.package_folder:
                continue
            name = str(dep).split("/")[0]
            print("Copying license files from", name, "to", os.path.join(self.build_folder, "licenses", name))
            copy(self,
                 pattern="license*",
                 src=dep.package_folder,
                 dst=os.path.join(self.build_folder, "licenses", name),
                 ignore_case=True,
                 keep_path=False)

    def requirements(self):
        self.requires("rapidjson/cci.20230929")
        self.requires("qt-advanced-docking-system/4.5.0")
        self.requires("sentry-breakpad/0.6.5")
        self.requires("protobuf/6.33.5")
        self.requires("abseil/20260107.1")

        if self.settings.os == "Windows":
            self.requires("boost/1.86.0")
            self.requires("qt/[>=6.8 <6.9]") #we are aiming for the latest LTS release here
        else:
            # Satisfied by the qt-system wrapper, which reports the actual installed version
            # via pkg-config. Requires at least Qt 6.4 (available in Ubuntu Noble).
            self.requires("qt/[>=6.4]")
