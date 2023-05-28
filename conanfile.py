""" Conan stuff for Safir SDK Core """
from conans import ConanFile

class SafirSdkCoreConan(ConanFile):
    """ Conan stuff for Safir SDK Core """
    settings = "os", "compiler", "build_type", "arch"
    generators = "cmake_find_package"
    default_options = {"websocketpp:asio": False,
                       "websocketpp:with_openssl": False,
                       "websocketpp:with_zlib":False,

                       "protobuf:lite":True,
                       "protobuf:shared":False,
                       "protobuf:with_zlib":False,

                       # All the options below are only used on Windows
                       "boost:shared": False,
                       "boost:layout":"versioned",
                       "boost:error_code_header_only":True,
                       "boost:system_no_deprecated":True,
                       "boost:filesystem_no_deprecated":True,
                       "boost:filesystem_version":3,
                       "boost:magic_autolink":False,
                       "boost:without_wave": True,
                       "boost:without_fiber": True,
                       "boost:without_context": True,
                       "boost:without_contract": True,
                       "boost:without_coroutine": True,
                       "boost:without_serialization": True,
                       "boost:without_stacktrace": True,
                       "boost:without_python": True,
                       "boost:without_graph": True,
                       "boost:without_graph_parallel": True,
                       "boost:without_math": True,
                       "boost:without_mpi": True,
                       "boost:without_json": True,
                       "boost:without_type_erasure": True,
                       "boost:without_url": True,
                       "boost:bzip2": False,
                       "boost:zlib": False,

                       "qt:shared": False,
                       "qt:commercial": False,
                       "qt:opengl": "no",
                       "qt:openssl": False,
                       "qt:with_vulkan": False,
                       "qt:with_pcre2": True,
                       "qt:with_glib": False,
                       "qt:with_doubleconversion": True,
                       "qt:with_freetype": False,
                       "qt:with_harfbuzz": False,
                       "qt:with_libjpeg": False,
                       "qt:with_libpng": True,
                       "qt:with_sqlite3": False,
                       "qt:with_mysql": False,
                       "qt:with_pq": False,
                       "qt:with_odbc": False,
                       "qt:with_openal": False,
                       "qt:with_zstd": False,
                       "qt:with_gstreamer": False,
                       "qt:with_pulseaudio": False,
                       "qt:with_dbus": False,
                       "qt:qtactiveqt": False,
                       "qt:qtcharts": False,
                       "qt:qtconnectivity": False,
                       "qt:qtdatavis3d": False,
                       "qt:qtdeclarative": False,
                       "qt:qtdoc": False,
                       "qt:qtimageformats": False,
                       "qt:qtlottie": False,
                       "qt:qtmultimedia": False,
                       "qt:qtnetworkauth": False,
                       "qt:qtquick3d": False,
                       "qt:qtquicktimeline": False,
                       "qt:qtremoteobjects": False,
                       "qt:qtscxml": False,
                       "qt:qtsensors": False,
                       "qt:qtserialbus": False,
                       "qt:qtserialport": False,
                       "qt:qtsvg": False,
                       "qt:qttools": False,
                       "qt:qttranslations": False,
                       "qt:qtvirtualkeyboard": False,
                       "qt:qtwebchannel": False,
                       "qt:qtwebengine": False,
                       "qt:qtwebsockets": False,
                       "qt:qtwebview": False,
                       "qt:gui": True,
                       "qt:widgets": True,
                       "pcre2:shared": False,
                       "pcre2:with_zlib":False,
                       "pcre2:with_bzip2": False
                       }

    def imports(self):
        self.copy("license*", dst="licenses", folder=True, ignore_case=True, keep_path=False)
        if self.settings.os == "Windows":
            self.copy("*.exe", src="bin", dst="bin", root_package="ninja")

    def requirements(self):
        self.requires("sentry-breakpad/0.6.2")
        self.requires("websocketpp/0.8.2")
        self.requires("rapidjson/cci.20220822")
        self.requires("protobuf/3.21.9")
        if self.settings.os == "Windows":
            self.requires("qt/5.15.8")
            self.requires("ninja/1.11.1")
            self.requires("boost/1.81.0")
