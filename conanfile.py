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
        # Every requirement below names an exact version AND an exact
        # ConanCenter recipe revision (the part after the '#'). The revision is
        # not decoration: ConanCenter republishes recipes for versions that
        # shipped long ago, and such a republish changes what we build without
        # changing a single character in this file. One on 2026-11-04 broke
        # every Linux build; see "The Linux `qt` Conan stub, and ConanCenter
        # revision drift" in AGENTS.md.
        #
        # So these are stepped up deliberately, when a release is cut, and not
        # otherwise - the recipe for doing that is "Dependency Pins, and
        # Stepping Them Up" in AGENTS.md. Read it before editing a line here;
        # a hash bumped on its own, mid-cycle, is exactly what the pins exist
        # to prevent.
        #
        # When the cmake build sets SAFIR_DUAL_ABI_ONLY (the dual-ABI-libs
        # only slice used by Windows release builds in their Debug pass), we
        # don't need Qt - none of the dual-ABI libraries depend on it.
        # Skipping the requirement avoids a from-source Qt build on machines
        # without it cached.
        dual_abi_only = self.conf.get("user.safir:dual_abi_only", default=False, check_type=bool)

        self.requires("rapidjson/cci.20250205#dc3e0ac7034c35032b107673d9f9bae3")
        self.requires("sentry-breakpad/0.6.5#a1d930d9a727a3326ec2734141e2cc78")
        self.requires("protobuf/7.36.2#29bfa9f880fc7d96945a4f2e54b71aa2")
        # abseil cannot move past this: both protobuf 6.33.5 and 7.36.2 require
        # abseil/[>=20240722.0 <=20260107.1], and we are already at the ceiling.
        # 20260526.0 and 20260817.0 exist on ConanCenter and are unusable until
        # protobuf's own range opens up - check it, not just the abseil list,
        # before trying again.
        self.requires("abseil/20260107.1#883e95a7be2b767999f64669ac642e5d")

        if not dual_abi_only:
            self.requires("qt-advanced-docking-system/5.1.1#45978fdc6718f7d2ed9875801a34ebbc")

        if self.settings.os == "Windows":
            # Still 1.86.0: the 1.91.0 step-up is blocked on Boost.Interprocess,
            # not on anything here. See #622 and "Boost 1.91 and the
            # shared-memory smart pointers" in AGENTS.md before trying again.
            self.requires("boost/1.86.0#78bd5be42f6cee6c859efae8352346c7")
            if not dual_abi_only:
                # The Qt LTS we ship on Windows. This used to be the range
                # [>=6.8 <6.9], which tracked the newest 6.8 patch by itself -
                # and since our trimmed options mean no ConanCenter binary ever
                # matches, a patch arriving mid-cycle silently bought a
                # from-source Qt rebuild and a different shipped Qt. Moving to
                # the next LTS series is now a release-time decision; which
                # series that is comes from qt.io, not from the ConanCenter
                # version list, which carries non-LTS versions too.
                self.requires("qt/6.8.3#6ef0549c5ab025900d94d9ce4c6da6c0")
        elif not dual_abi_only:
            # The one requirement that cannot carry a pin, by construction.
            # On Linux this resolves to build/conan-recipes/qt-system, a local
            # stub that versions itself from `pkg-config --modversion Qt6Core`
            # - so it is 6.4.2 on Ubuntu Noble and 6.8.2 on Debian Trixie, and
            # no single version, let alone revision, can name both. The floor
            # is the oldest distro Qt we support (Noble's 6.4).
            self.requires("qt/[>=6.4]")
