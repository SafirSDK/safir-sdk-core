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

                       # Qt and its pcre2 dependency are configured in the
                       # build/conan-profiles/windows-qt profile, NOT here.
                       # Qt appears in both the host context (the Qt we ship) and
                       # the build context (tool_required for moc/rcc/uic), but
                       # recipe default_options only affect the host context. The
                       # profile is applied to both contexts so a single, trimmed
                       # Qt is built once instead of a second full Qt (with
                       # glib/harfbuzz/openssl/sqlite3/libpq/...) from source.
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
        # When the cmake build sets SAFIR_DUAL_ABI_ONLY (the dual-ABI-libs
        # only slice used by Windows release builds in their Debug pass), we
        # don't need Qt - none of the dual-ABI libraries depend on it.
        # Skipping the requirement avoids a from-source Qt build on machines
        # without it cached.
        dual_abi_only = self.conf.get("user.safir:dual_abi_only", default=False, check_type=bool)

        self.requires("rapidjson/cci.20230929")
        self.requires("sentry-breakpad/0.6.5")
        self.requires("protobuf/6.33.5")
        self.requires("abseil/20260107.1")

        if not dual_abi_only:
            self.requires("qt-advanced-docking-system/4.5.0")

        if self.settings.os == "Windows":
            self.requires("boost/1.86.0")
            if not dual_abi_only:
                self.requires("qt/[>=6.8 <6.9]") #we are aiming for the latest LTS release here
        elif not dual_abi_only:
            # Satisfied by the qt-system wrapper, which reports the actual installed version
            # via pkg-config. Requires at least Qt 6.4 (available in Ubuntu Noble).
            self.requires("qt/[>=6.4]")
