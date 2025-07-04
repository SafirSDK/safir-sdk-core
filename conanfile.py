""" Conan stuff for Safir SDK Core """
from conan import ConanFile
from conan.tools.files import copy
import os

class SafirSdkCoreConan(ConanFile):
    """ Conan stuff for Safir SDK Core """
    settings = "os", "compiler", "build_type", "arch"
    generators = "CMakeDeps"
    default_options = {"websocketpp/*:asio": False,
                       "websocketpp/*:with_openssl": False,
                       "websocketpp/*:with_zlib":False,

                       "protobuf/*:lite":True,
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
                       "boost/*:without_context": True,
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
                       "pcre2/*:with_bzip2": False
                       }
    def configure(self):
        if self.settings.os == "Windows":
            self.options["qt-advanced-docking-system"].qt_from_conan = True
            if self.settings.arch == "x86" or \
               self.settings.compiler.version == 190 or \
               self.settings.compiler.version == 191:
                self.options["qt-advanced-docking-system"].qt_major_version = 5


    def generate(self):
        for dep in self.dependencies.values():
            name = str(dep).split("/")[0]
            print("Copying license files from", name, "to", os.path.join(self.build_folder, "licenses", name))
            copy(self,
                 pattern="license*",
                 src=dep.package_folder,
                 dst=os.path.join(self.build_folder, "licenses", name),
                 ignore_case=True,
                 keep_path=False)
            if "ninja" == name and self.settings.os == "Windows":
                print("Copying ninja executable from", name, "to", os.path.join(self.build_folder, "bin"))
                copy(self, "ninja.exe", src=dep.cpp_info.bindirs[0], dst=os.path.join(self.build_folder, "bin"))

    def requirements(self):
        self.requires("websocketpp/0.8.2")
        self.requires("rapidjson/cci.20230929")
        self.requires("ninja/1.12.1")
        self.requires("qt-advanced-docking-system/4.4.0")
        #Visual Studio 2015 does not compile the latest sentry-breakpad (lacks c++17 support).
        #0.5.3 appears to be the last one that doesn't need that. Even 0.5.4 wants it.
        if self.settings.os == "Windows" and self.settings.compiler.version == 190:
            self.requires("sentry-breakpad/0.5.3")
        else:
            self.requires("sentry-breakpad/0.6.5")

        #VS2015 has to use older protobuf due to missing c++14 support
        if self.settings.os == "Windows" and self.settings.compiler.version == 190:
            self.requires("protobuf/3.21.12")
        else:
            self.requires("protobuf/5.27.0")
            #newer abseils require cmake from conan, which is not available on
            #for example x86 platforms. So we use an older one for now.
            self.requires("abseil/20240116.2")

        if self.settings.os == "Windows":
            #VS2015 does not build boost 1.85 for some reason, so we use an older version for that.
            if self.settings.compiler.version == 190:
               self.requires("boost/1.83.0")
            else:
                self.requires("boost/1.86.0")

            #Visual Studio 2015 and 2017 does not have support for c++17, which is required
            #by qt6. So we go for qt5 instead there.
            #The conan recipe for qt6 does not work for x86 currently, so we fall back to qt5
            if self.settings.arch == "x86" or \
               self.settings.compiler.version == 190 or \
               self.settings.compiler.version == 191:
                self.requires("qt/5.15.14")
            else:
                self.requires("qt/6.7.3")

