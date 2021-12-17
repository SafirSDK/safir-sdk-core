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
                       "boost:shared": True,
                       "boost:layout":"versioned",
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
                       "boost:bzip2": True,
                       "boost:zlib": True}

    def imports(self):
        if self.settings.os == "Windows":
            self.copy("*.dll", src="bin", dst="./bin", root_package="boost")
            self.copy("*.lib", src="lib", dst="./lib", root_package="boost")
            self.copy("*.hpp", src="include", dst="./include", root_package="boost")

    def requirements(self):
        self.requires("sentry-breakpad/0.4.12")
        self.requires("websocketpp/0.8.2")
        self.requires("rapidjson/cci.20200410")
        protobuf_version = "3.17.1"
        if self.settings.os == "Windows":
            self.requires("boost/1.77.0")
            if self.settings.compiler.version == 14:
                protobuf_version = "[<3.15]"
        self.requires(f"protobuf/{protobuf_version}")

    def configure(self):
        pass
        #self.options = False

