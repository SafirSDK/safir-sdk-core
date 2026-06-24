#!/usr/bin/env python3
# -*- coding: utf-8 -*-
###############################################################################
#
# Copyright Saab AB, 2009-2014 (http://safirsdkcore.com)
#
# Created by: Lars Hagstrom / lars.hagstrom@consoden.se
#
###############################################################################
#
# This file is part of Safir SDK Core.
#
# Safir SDK Core is free software: you can redistribute it and/or modify
# it under the terms of version 3 of the GNU General Public License as
# published by the Free Software Foundation.
#
# Safir SDK Core is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with Safir SDK Core.  If not, see <http://www.gnu.org/licenses/>.
#
###############################################################################
"""Shared build logic for safir-sdk-core.

This module is imported by the two entry-point scripts that live alongside it:
  * build.py        - builds and packages the Safir SDK Core source tree
                      (used by Jenkins and developers).
  * dobmake_batch.py - builds an external user dou-project (installed into the
                      SDK as 'dobmake-batch' and driven by the dobmake GUI).

Both use cases need the same Visual Studio environment setup and cmake
build/install plumbing, which is why that lives here. They differ in intent:
build.py always packages (it sets arguments.package = True itself), while
dobmake_batch.py never does (it offers --install/--clean instead and leaves
package defaulted to False). That is why the dual-ABI packaging fast-path can
never be reached from a dobmake build.

The module finds itself on sys.path because Python prepends the running
script's own directory to sys.path[0]; build.py/dobmake_batch.py and this file
are always installed/checked-out side by side.
"""
import os
import glob
import sys
import subprocess
import platform
import xml.dom.minidom
import re
import time
import shutil
import argparse
import codecs
import contextlib
from xml.sax.saxutils import escape

from os.path import join, isfile, isdir

try:
    from shutil import which
except:
    from distutils.spawn import find_executable as which

#Make linux_distribution available from some suitable package, falling back to returning that we don't know.
try:
    from distro import linux_distribution
except:
    try:
        from platform import linux_distribution
    except:
        def linux_distribution():
            return ("unknown",)

#a few constants
known_configs = set(["Release", "Debug", "MinSizeRel", "RelWithDebInfo"])

#The Visual Studio versions we support. Older versions (vs2015/2017/2019) have
#been dropped, which is why all the registry/legacy-vcvarsall handling is gone.
supported_studios = ("vs2022", "vs2026")


class FatalError(Exception):
    """our own exception for "die" fcn"""


def die(message):
    """Just raise an exception with a message"""
    raise FatalError(message)


def is_64_bit():
    """Detecting this is a lot more complex than it should be.
    See http://stackoverflow.com/questions/2764356/python-get-windows-os-version-and-architecture
    and http://bytes.com/topic/python/answers/509764-detecting-64bit-vs-32bit-linux
    This will work reasonably well on our supported systems:"""
    if sys.platform.startswith("linux"):
        return platform.architecture()[0] == "64bit"
    else:
        processor_architecture = os.environ.get("PROCESSOR_ARCHITECTURE")
        processor_architew6432 = os.environ.get("PROCESSOR_ARCHITEW6432")
        return processor_architecture == "AMD64" or processor_architew6432 == "AMD64"


def mkdir(newdir):
    """works the way a good mkdir should :)
        - already exists, silently complete
        - regular file in the way, raise an exception
        - parent directory(ies) does not exist, make them as well
    """
    if os.path.isdir(newdir):
        pass
    elif os.path.isfile(newdir):
        raise OSError("a file with the same name as the desired dir, '{}', already exists.".format(newdir))
    else:
        head, tail = os.path.split(newdir)
        if head and not os.path.isdir(head):
            mkdir(head)
        if tail:
            os.mkdir(newdir)


def remove(path):
    """Remove a file or directory recursively"""
    if not os.path.exists(path):
        return
    if os.path.isfile(path) or os.path.islink(path):
        try:
            os.remove(path)
            return
        except OSError as exc:
            die("Failed to remove file {}. Got exception {}".format(path,str(exc)))

    for name in os.listdir(path):
        if os.path.isdir(os.path.join(path, name)):
            remove(os.path.join(path, name))
        else:
            try:
                os.remove(os.path.join(path, name))
            except OSError as exc:
                die("Failed to remove file " + os.path.join(path, name) + ". Got exception " + str(exc))

    try:
        os.rmdir(path)
    except OSError as exc:
        die("Failed to remove directory " + path + ". Got exception " + str(exc))


def num_cpus():
    """Detects the number of CPUs on a system."""
    # Linux, Unix and MacOS:
    if hasattr(os, "sysconf"):
        if "SC_NPROCESSORS_ONLN" in os.sysconf_names:
            # Linux & Unix:
            ncpus = os.sysconf("SC_NPROCESSORS_ONLN")
            if isinstance(ncpus, int) and ncpus > 0:
                return ncpus
    # Windows:
    if "NUMBER_OF_PROCESSORS" in os.environ:
        ncpus = int(os.environ["NUMBER_OF_PROCESSORS"])
        if ncpus > 0:
            return ncpus
    return 1  # Default


def physical_memory():
    """Detect physical memory in computer"""
    if sys.platform.startswith("linux"):
        with open("/proc/meminfo", "rb") as a_file:
            meminfo = a_file.read().decode("ascii")
        match = re.search(r"MemTotal:\s*([0-9]*) kB", meminfo)
        return int(match.group(1)) / 1024
    else:
        return None


def num_jobs():
    """We need to limit ourselves a little bit in how many parallel jobs we perform. Each job may use
    up to 400Mb of memory."""
    try:
        num = num_cpus() + 1

        mem_per_job = 400
        memory = physical_memory()
        if memory is not None and memory / num < mem_per_job:
            num = max(1, int(memory / mem_per_job))
    except:  # pylint: disable=bare-except
        num = 2
    return num


@contextlib.contextmanager
def pushd(new_dir):
    previous_dir = os.getcwd()
    os.chdir(new_dir)
    try:
        yield
    finally:
        os.chdir(previous_dir)

def get_git_revision_info():
    """Collect git revision details from the current checkout.

    Returns a dict of strings, or None if this is not a usable git checkout.
    Used to inject the revision into the Debian package build: that build
    compiles from an extracted "git archive" tarball that has no .git of its
    own, so it cannot query git itself (see lluf_config_dump CMakeLists.txt)."""
    def _git(args):
        try:
            return subprocess.check_output(
                ["git"] + args, stderr=subprocess.DEVNULL,
                universal_newlines=True).strip()
        except (subprocess.CalledProcessError, FileNotFoundError):
            return None

    short = _git(["describe", "--always", "--dirty", "--abbrev=7"])
    if short is None:
        return None

    dirty = subprocess.call(["git", "diff-index", "--quiet", "HEAD", "--"],
                            stderr=subprocess.DEVNULL)
    return {
        "SAFIR_GIT_REVISION": short,
        "SAFIR_GIT_REVISION_FULL": _git(["rev-parse", "HEAD"]) or "Unknown",
        "SAFIR_GIT_BRANCH": _git(["rev-parse", "--abbrev-ref", "HEAD"]) or "Unknown",
        "SAFIR_GIT_STATUS": "dirty - uncommitted changes" if dirty else "clean",
    }


def _head_is_on_release_tag():
    """True if HEAD is exactly on a git tag, i.e. this is a release build.

    Uses --tags so lightweight tags count (release tags are created with plain
    `git tag`). Returns False if git is unavailable or HEAD is not on a tag.
    """
    try:
        subprocess.check_output(
            ["git", "describe", "--tags", "--exact-match", "HEAD"],
            stderr=subprocess.DEVNULL,
            universal_newlines=True
        )
        return True
    except (subprocess.CalledProcessError, FileNotFoundError):
        return False


def read_version():
    """Parse the VERSION.txt file to find out our version"""
    parts = {}
    with open("VERSION.txt", 'r') as version_file:
        for line in version_file:
            line = line.strip()
            if len(line) == 0 or line.startswith("#"):
                continue
            key, value = line.split("=")
            parts[key] = value

    version_string = parts["MAJOR"] + "." + parts["MINOR"] + "." + parts["PATCH"] + parts["SUFFIX"]

    # For non-release builds with a pre-release SUFFIX, append the git revision so
    # successive dev builds of the same alpha/beta are distinguishable. A build
    # whose HEAD sits exactly on a tag is a release (the tag identifies it), so
    # keep the version clean - just like stable releases, which use an empty
    # SUFFIX and never reach here. This keeps the git hash out of release
    # artifact names (e.g. the Windows installer filename).
    if parts["SUFFIX"] != "" and not _head_is_on_release_tag():
        try:
            git_revision = subprocess.check_output(
                ["git", "describe", "--always", "--dirty", "--abbrev=7"],
                stderr=subprocess.DEVNULL,
                universal_newlines=True
            ).strip()
            version_string += "-" + git_revision
        except (subprocess.CalledProcessError, FileNotFoundError):
            # Git not available or not a git repo, continue without git info
            pass

    return ((parts["MAJOR"], parts["MINOR"], parts["PATCH"], parts["SUFFIX"]), version_string)


class DummyLogger():
    """A logger that can be used until we know what actual logger should be used.
    It only logs to stdout"""
    @staticmethod
    def log(data, tag=None):
        """Log some data, to stdout in this case"""
        sys.stdout.write("{}{}\n".format((tag+': ') if tag is not None else '',data))
        sys.stdout.flush()

    @staticmethod
    def close():
        """Close the logger"""

    @staticmethod
    def log_output(process):
        """This logger cant actually do this"""
        raise Exception("DummyLogger doesnt support process output logging. " +
                        "You should investigate why the real logger is not instantiated by now...")


class Logger():
    """The main logger"""
    LogLevel = ("Brief", "Verbose")
    Tags = set(["header", "brief", "normal", "detail", "command_description", "command", "output"])

    def __init__(self, level):
        if level not in Logger.LogLevel:
            die("Bad log level")
        self.__log_level = level
        self.__last_tag = None
        if hasattr(sys.stdout, "reconfigure"):
            sys.stdout.reconfigure(encoding="utf-8", errors="replace")
        self.__buildlog = codecs.open("buildlog.html", mode="w", encoding="utf-8", errors="replace")
        self.__buildlog.write("<html><head>"
                              "<script type=\"text/javascript\">"
                              "function refreshPage () {"
                              "var page_y = document.getElementsByTagName(\"body\")[0].scrollTop;"
                              "window.location.href = window.location.href.split('?')[0] + '?page_y=' + page_y;"
                              "}"
                              "window.onload = function () {"
                              "setTimeout(refreshPage, 10000);"
                              "if ( window.location.href.indexOf('page_y') != -1 ) {"
                              "var match = window.location.href.split('?')[1].split(\"&\")[0].split(\"=\");"
                              "document.getElementsByTagName(\"body\")[0].scrollTop = match[1];"
                              "}"
                              "}"
                              "</script>"
                              "<title>Safir SDK Core Build Log</title>")
        self.__buildlog.write("<body>\n")
        self.__buildlog.write("<h1>Safir SDK Core Build Log</h1>")
        self.__buildlog.write("<b>Command line:</b> " + " ".join(sys.argv) + "<br/>")
        self.__buildlog.write("<b>Working directory:</b> " + os.getcwd() + "<br/>")
        self.__buildlog.write("<b>Start time (local time)</b>: " + time.asctime() + "<br/>")
        self.__buildlog.write("<h2>Starting build</h2>\n")

    def close(self):
        """Close the log"""
        self.__buildlog.write("\n<p/>End time (local time): " + time.asctime())
        self.__buildlog.write("\n</body>\n")
        self.__buildlog.close()

    @staticmethod
    def __print(data):
        try:
            sys.stdout.write(data)
            sys.stdout.write("\n")
            sys.stdout.flush()
        except UnicodeEncodeError:
            sys.stdout.write("Failed to decode something in data to be printed. Sorry.\n")
            sys.stdout.flush()

    def __log_stdout(self, data, tag):
        if tag not in Logger.Tags:
            die("unknown logging tag")
        if self.__log_level == "Brief":
            if tag in ("header", "normal", "brief"):
                self.__print(data)
        elif self.__log_level == "Verbose":
            if tag == "brief":
                pass
            elif tag == "header":
                self.__print("\n==== " + data + " ====")
            elif tag == "command_description":
                self.__print("+ " + data + ": ")
            elif tag == "command":
                self.__print("'" + data + "'")
            else:
                self.__print(data)

    def __log_file(self, data, tag):
        log = self.__buildlog

        if self.__last_tag == "output" and self.__last_tag != tag:
            log.write("</pre>\n")

        if tag == "header":
            log.write("<h3>" + data + "</h3>\n")
        elif tag == "brief":
            pass
        elif tag == "normal":
            log.write(data + "<br/>\n")
        elif tag == "detail":
            log.write(data + "<br/>\n")
        if tag == "command_description":
            log.write("<h4>" + data + "</h4>\n")
        elif tag == "command":
            log.write("<pre style=\"color: green\">" + data + "</pre>\n")
        elif tag == "output":
            if self.__last_tag != tag:
                log.write("<pre>" + data)
            else:
                log.write("\n" + data)
        log.flush()
        self.__last_tag = tag

    def log(self, data, tag="normal"):
        """log some data"""
        if data is None:
            return

        self.__log_stdout(data, tag)
        self.__log_file(data, tag)

    def log_output(self, process):
        """log the output of a process"""
        output = []
        while True:
            line = process.stdout.readline()
            if not line:
                break
            #CMake does some strange thing with a carriage return alone on a line, which we get rid of like this.
            line = line.decode("utf8", errors="replace").rstrip("\r")
            if len(line) != 0:
                line = line.rstrip()
                self.log(line, "output")
                output += (line, )
        process.wait()
        if process.returncode != 0:
            self.log("Failure, return code is " + str(process.returncode))
        self.log("", "output")
        return "\n".join(output)


# create a dummy logger that we use until we have the real thing. finalize_arguments()
# replaces this with a real Logger once we know the desired verbosity.
LOGGER = DummyLogger()


def add_common_arguments(parser):
    """Add the arguments shared by all entry points."""
    parser.add_argument("--no-unity-build",
                        action="store_true",
                        help="Unity builds can require a lot of memory for the compiler. Try this "
                        "if you are having trouble with dobmake crashing due to internal compiler "
                        "errors. You may have to clean out previous results first.")

    parser.add_argument("--skip-tests", action="store_true", help="Skip running the unit tests")

    parser.add_argument("--jenkins",
                        action="store_true",
                        help="Increase verbosity and obey build matrix variables.")

    parser.add_argument("--verbose",
                        "-v",
                        action="count",
                        default=0,
                        help="Print more stuff about what is going on. Use twice to get very verbose output.")


def add_win32_options(parser):
    """add windows options to the parser"""
    parser.add_argument("--use-studio",
                        help="The visual studio to use for building (vs2022, vs2026)",
                        action="store")
    parser.add_argument("--arch",
                        default="amd64" if is_64_bit() else "x86",
                        choices=["x86","amd64"],
                        help="Architecture to build. Note that you may not be able to run tests if you cross-compile to an arch you can't run.")
    parser.add_argument("--32-bit",
                        action="store_true",
                        help=argparse.SUPPRESS)
    parser.add_argument("--configs",
                        default=("Debug", "RelWithDebInfo"),
                        nargs='*',
                        choices=known_configs,
                        help="The configurations to build. Debug and RelWithDebInfo is the default.")


def add_linux_options(parser):
    """Add linux opitons to the parser"""
    parser.add_argument("--config",
                        dest="configs",
                        nargs=1,
                        default=("RelWithDebInfo", ),
                        choices=known_configs,
                        help="The configuration to build. RelWithDebInfo is the default.")


def add_platform_options(parser):
    """Add the options that depend on the platform we're running on."""
    if sys.platform.startswith("linux"):
        add_linux_options(parser)
    else:
        add_win32_options(parser)


def finalize_arguments(arguments):
    """Apply post-parse fixups and create the real logger. Both entry points call
    this after parsing. It also fills in defaults for the package/install/clean
    attributes, so that the shared builder code can reference them unconditionally
    regardless of which entry point set them (build.py sets package itself;
    dobmake_batch.py exposes --install/--clean but not --package)."""
    for attr, default in (("package", False), ("package_noclean", False),
                          ("install", None), ("clean", False)):
        if not hasattr(arguments, attr):
            setattr(arguments, attr, default)

    if arguments.jenkins:
        arguments.verbose += 1
    if arguments.verbose >= 2:
        os.environ["VERBOSE"] = "1"
    if arguments.package_noclean:
        arguments.package = True

    if sys.platform == "win32":
        if vars(arguments)["32_bit"]:
            arguments.arch = "x86"

        if arguments.use_studio not in supported_studios:
            die("Unknown studio version '" + str(arguments.use_studio) +
                "'. Supported versions are: " + ", ".join(supported_studios))

    global LOGGER
    LOGGER = Logger("Brief" if arguments.verbose == 0 else "Verbose")


class BuilderBase(object):
    """Base class for builders"""
    def __init__(self, arguments):
        self.num_jobs = num_jobs()

        # We want Ninja for building
        if which("ninja") is None:
            die("Need ninja to build!")

        self.total_tests = 0
        self.failed_tests = 0

        self.arguments = arguments
        self.__handle_command_line_arguments()

    def __handle_command_line_arguments(self):
        self.configs = self.arguments.configs

        self.debug_only = False
        if self.arguments.jenkins:
            if os.environ.get("PACKAGE_TYPE") == "DebugOnly":
                LOGGER.log("Using Config 'DebugOnly', building everything in Debug only.")
                self.configs = ("Debug", )
                self.debug_only = True

        self.stagedir = os.path.join(os.getcwd(), "stage") if self.arguments.package else None

        self.install_prefix = None  #derived classes can override if arguments.package is true
        if self.arguments.install and self.arguments.install != "None":
            self.install_prefix = self.arguments.install

    def _supports_dual_abi_fast_path(self):
        """Override in builders where the Debug pass of a packaging build only needs to
        produce the dual-ABI C++ shared libraries (the MSVC debug/release runtime split).
        On other platforms there is no ABI split, so the full tree must be built."""
        return False

    def build(self):
        """Build the project"""
        # In a Windows packaging build that produces both the Debug and RelWithDebInfo
        # flavours, the Debug pass only needs to build the dual-ABI C++ libraries (the
        # ones whose MSVC runtime must match a debug-built consumer). Everything else is
        # identical between the two passes and gets overwritten, so building it twice is
        # wasted work. See src/cmake/SafirLibraryAbi.cmake.
        dual_abi_pair = (self.arguments.package
                         and not self.debug_only
                         and self._supports_dual_abi_fast_path()
                         and set(self.configs) == {"Debug", "RelWithDebInfo"})

        for config in self.configs:
            if self.arguments.clean:
                remove(config)
                continue
            mkdir(config)
            dual_abi_only = dual_abi_pair and config == "Debug"
            with pushd(config):
                self.__build_internal(os.pardir, config, dual_abi_only)

        if self.arguments.package:
            self.__package()

    def __package(self):
        LOGGER.log("Building installation package", "header")
        self.stage_package()

    def __configure(self, srcdir, config, dual_abi_only):

        command = ("cmake", "-G", "Ninja", "-D", "CMAKE_BUILD_TYPE:string=" + config)

        command += ("-D", f"NO_SAFIR_UNITY_BUILD={self.arguments.no_unity_build}")

        if dual_abi_only:
            command += ("-D", "SAFIR_DUAL_ABI_ONLY=ON")

        if self.install_prefix is not None:
            command += ("-D", "CMAKE_INSTALL_PREFIX=" + self.install_prefix)

        command += (srcdir, )
        self._run_command(command, "Configure for " + config + " build")

    def __build_internal(self, srcdir, config, dual_abi_only=False):
        if dual_abi_only:
            LOGGER.log(" - in config " + config + " (dual-ABI C++ libraries only)", "brief")
        else:
            LOGGER.log(" - in config " + config, "brief")

        self.__configure(srcdir, config, dual_abi_only)

        command = ("cmake", "--build", ".")

        if dual_abi_only:
            command += ("--target", "safir_dual_abi_libs")

        if self.arguments.verbose >= 2:
            command += ("-v",)

        self._run_command(command, "Build " + config)
        if not self.arguments.skip_tests and not dual_abi_only:
            LOGGER.log("   + testing", "brief")
            self.test()
            translate_results_to_junit(config)

        if self.arguments.package:
            LOGGER.log("   + installing to staging area", "brief")
            self.__stage_install()
        elif self.arguments.install:
            self.__install()

    def __stage_install(self):
        for component in ("Runtime", "Tools", "Development", "TestSuite"):
            command = ("cmake", "-DCOMPONENT=" + component, "-P", "cmake_install.cmake")
            env = os.environ.copy()
            env["DESTDIR"] = os.path.join(self.stagedir, component)
            self._run_command(command, "Staged install " + component, env=env)

    def __install(self):
        command = ("cmake", "-P", "cmake_install.cmake")
        self._run_command(command, "Installing to " + self.arguments.install)

    def stage_package(self):
        LOGGER.log(" ! Packaging not implemented in this builder !", "brief")

    def test(self):
        """run ctest in current directory"""
        if not os.path.isfile("DartConfiguration.tcl"):
            dummyfile = open("DartConfiguration.tcl", "w")
            dummyfile.close()

        output = self._run_command(("ctest", "--output-on-failure", "-T", "Test", "--no-compress-output"),
                                   "Test",
                                   allow_fail=True)
        self.interpret_test_output(output)

    def _run_command(self, cmd, description, allow_fail=False, env=None):
        """Run a command"""

        LOGGER.log(description, "command_description")
        LOGGER.log(" ".join(cmd), "command")

        process = subprocess.Popen(cmd, stdout=subprocess.PIPE, stderr=subprocess.STDOUT, env=env)
        output = LOGGER.log_output(process)
        if process.returncode != 0:
            if not allow_fail:
                die("Failed to run '" + " ".join(cmd) + "' in " + os.getcwd())
            else:
                LOGGER.log("This command failed, but failure of this particular command " +
                           "is non-fatal to the build process, so I'm continuing")

        return output

    def interpret_test_output(self, output):
        LOGGER.log("Checking test output", "detail")
        match = re.search(r"tests passed, ([0-9]+) tests failed out of ([0-9]+)", output)
        if not match:
            if output.find("No tests were found") == -1:
                LOGGER.log("Failed to parse test output!")
            return
        self.total_tests += int(match.group(2))
        self.failed_tests += int(match.group(1))


class VisualStudioBuilder(BuilderBase):
    def __init__(self, arguments):
        super(VisualStudioBuilder, self).__init__(arguments)

        self.install_target = "Install"

        self.__setup_build_environment()

        #Disable msvc leak detection in boost test unit tests. This detects loads of spurious memory
        #leaks, which makes it useless.
        os.environ["BOOST_TEST_DETECT_MEMORY_LEAK"] = "0"
    @staticmethod
    def can_use():
        return sys.platform == "win32"

    def _supports_dual_abi_fast_path(self):
        #The dual/single ABI split only matters for the MSVC debug/release runtimes.
        return True

    @staticmethod
    def __find_vcvarsall():
        """Locate vcvarsall.bat for Visual Studio 2022 or later using vswhere."""
        root = os.environ.get("ProgramFiles(x86)") or os.environ.get("ProgramFiles")
        if not root:
            return None

        try:
            path = subprocess.check_output([
                join(root, "Microsoft Visual Studio", "Installer", "vswhere.exe"),
                "-latest",
                "-prerelease",
                "-requiresAny",
                "-requires", "Microsoft.VisualStudio.Component.VC.Tools.x86.x64",
                "-requires", "Microsoft.VisualStudio.Workload.WDExpress",
                "-property", "installationPath",
                "-products", "*",
            ]).decode(encoding="mbcs", errors="strict").strip()
        except (subprocess.CalledProcessError, OSError, UnicodeDecodeError):
            return None

        vcvarsall = join(path, "VC", "Auxiliary", "Build", "vcvarsall.bat")
        if isfile(vcvarsall):
            return vcvarsall

        return None

    def __run_vcvarsall(self, vcvarsall):
        #Map the requested Visual Studio to the matching VC toolset version. Note that
        #a single VS install can carry several toolsets, so we pin the one we want.
        vcver = {"vs2022": "14.4",  #vc 17.10.4 or later
                 "vs2026": "14.5"}.get(self.arguments.use_studio)
        if vcver is None:
            die("Unsupported Visual Studio version: " + str(self.arguments.use_studio))

        cmd = '"{}" {} -vcvars_ver={} & set'.format(vcvarsall, self.arguments.arch, vcver)

        LOGGER.log("Running '" + cmd + "' to extract environment")
        proc = subprocess.Popen(cmd, stdout=subprocess.PIPE, stderr=subprocess.STDOUT, universal_newlines=True)
        output = proc.communicate()[0]
        if proc.returncode != 0:
            die("Failed to fetch environment variables out of vcvarsall.bat: " + output)
        return output

    def __setup_build_environment(self):
        """
        Find vcvarsall.bat and load the relevant environment variables from it.  This function
        is inspired (but not copied, for licensing reasons) by the one in python's setuptools.
        """

        vcvarsall = self.__find_vcvarsall()
        if not vcvarsall:
            die("Could not find vcvarsall.bat. Visual Studio 2022 or later is required.")

        #use uppercase only in this variable!
        required_variables = set(["LIB", "LIBPATH", "PATH", "INCLUDE", "VSINSTALLDIR"])
        optional_variables = set([
            "PLATFORM",
        ])
        wanted_variables = required_variables | optional_variables  #union

        LOGGER.log("Loading Visual Studio Environment", "header")
        output = self.__run_vcvarsall(vcvarsall)

        found_variables = set()

        for line in output.split("\n"):
            if '=' not in line:
                continue
            line = line.strip()
            name, value = line.split('=', 1)
            name = name.upper()
            if name in wanted_variables:
                if value.endswith(os.pathsep):
                    value = value[:-1]
                if os.environ.get(name) is None:
                    LOGGER.log("Will set '" + name + "' to '" + value + "'", "detail")
                else:
                    LOGGER.log("Will change '" + name + "' from '" + os.environ.get(name) + "' to '" + value + "'",
                               "detail")
                os.environ[name] = value
                found_variables.add(name)

        if len(required_variables - found_variables) != 0:
            die("Failed to find all expected variables in vcvarsall.bat")

        self.__conan_tweaks()

        #we also need to put the java 32 bit vm first in the path, if we're running an x86 build
        dont_build_java = os.getenv("SAFIR_DONT_BUILD_JAVA", 'False').lower() in ('true', '1', 't')
        if not dont_build_java and self.arguments.arch == "x86":
            for path in os.environ["PATH"].split(";"):
                candidate = os.path.join(path,"java.exe")
                if os.path.exists(candidate):
                    try:
                        res = subprocess.check_output((candidate,"-XshowSettings:properties","-version"),
                                                      stderr=subprocess.STDOUT).decode(encoding="mbcs",
                                                                                       errors="replace").strip()
                        if "sun.arch.data.model = 32" in res:
                            LOGGER.log("Will add '" + path + "' to beginning of PATH", "detail")
                            os.environ["PATH"] = path + ";" + os.environ["PATH"]
                            break
                    except subprocess.SubprocessError as e:
                        LOGGER.log("Failed to check whether JDK iss 32 or 64 bit, Java part of the build may not " +
                                   "work. Consider setting environment variable SAFIR_DONT_BUILD_JAVA to 1 to " +
                                   "avoid building java interfaces altogether.")

    def __conan_tweaks(self):
        """
        Here are some tweaks to allow us to build with vc versions that are installed
        under a newer Visual Studio version. For example VS2022 can have an older Visual C++
        toolset installed. This tells conan which IDE version we're using so that its
        packages build with a matching configuration.
        """
        if not os.path.exists("conanfile.py"):
            return

        ide_version = {"vs2022": 17,
                       "vs2026": 18}.get(self.arguments.use_studio)
        if ide_version is None:
            die("Unsupported Visual Studio version: " + str(self.arguments.use_studio))

        arg = f"--conf:all tools.microsoft.msbuild:vs_version={ide_version}"
        os.environ["CONAN_EXTRA_ARGUMENTS"] = arg
        LOGGER.log(f"Setting environment variable CONAN_EXTRA_ARGUMENTS to '{arg}', " +
                   "to encourage conan packages to build correctly.")


    def stage_package(self):
        _, version_string = read_version()

        #If we're cross compiling we need to rename directories a bit.
        if is_64_bit() and self.arguments.arch == "x86":
            for base in ["Runtime", "Development", "Tools", "TestSuite"]:
                os.rename(os.path.join(self.stagedir,base,"Program Files (x86)"), os.path.join(self.stagedir,base,"Program Files"))

        #Convert arch string to nsis format
        arch = self.arguments.arch
        if self.arguments.arch == "amd64":
            arch = "x86-64"

        command = ("makensis", "/DARCH=" + arch, "/DSTUDIO=" + self.arguments.use_studio.replace("vs",""),
                   "/DVERSION=" + version_string)

        if self.debug_only:
            command += ("/DDEBUGONLY", )

        command += (os.path.join("build", "packaging", "windows", "installer.nsi"), )

        self._run_command(command, "Packaging ")


class UnixGccBuilder(BuilderBase):
    def __init__(self, arguments):
        super(UnixGccBuilder,self).__init__(arguments)

        self.install_target = "install"

    @staticmethod
    def can_use():
        return sys.platform.startswith("linux")

class DebianPackager():
    """this builder has nothing in common with the other builders, really. Which is why it does
       not inherit from BuilderBase..."""
    def __init__(self, arguments):
        self.num_jobs = num_jobs()

        #this builder doesnt support exposing test results.
        self.total_tests = -1
        self.failed_tests = -1

        self.arguments = arguments
        if len(self.arguments.configs) != 1:
            die("DebianPackager can only build one config")
        if which("conan") is None:
            die("Could not find conan executable")

        self.noclean = arguments.package_noclean and os.path.exists("tmp")

        if self.arguments.jenkins:
            if os.environ.get("PACKAGE_TYPE") == "DebugOnly":
                LOGGER.log("Using Config 'DebugOnly', building everything in Debug only.")
                self.arguments.configs = ("Debug", )

    @staticmethod
    def can_use():
        """Can be used on debian based distros"""
        return sys.platform.startswith("linux") and \
            linux_distribution()[0] in ("Debian GNU/Linux", "Ubuntu")

    @staticmethod
    def __run(cmd, description):
        """Run a command"""

        LOGGER.log(description, "command_description")
        LOGGER.log(" ".join(cmd), "command")

        with subprocess.Popen(cmd, stdout=subprocess.PIPE, stderr=subprocess.STDOUT) as process:
            output = LOGGER.log_output(process)
            if process.returncode != 0:
                die("Failed to run '" + " ".join(cmd) + "' in " + os.getcwd())

        return output

    def build(self):
        """Run the build"""
        (major, minor, patch, suffix), _ = read_version()
        version_string = major + "." + minor + "." + patch + suffix
        # Capture the revision from the real checkout now, before we descend
        # into the extracted tarball (which has no .git), so we can hand it to
        # the package build via debuild below.
        git_info = get_git_revision_info()
        if not self.noclean:
            remove("tmp")
            mkdir("tmp")
            self.__run(("git", "archive",
                        "--prefix", "safir-sdk-core_" + version_string + "/",
                        "-o", "tmp/safir-sdk-core_" + version_string + ".orig.tar.gz",
                        "HEAD"), "creating tar archive")
        os.chdir("tmp")
        if not self.noclean:
            self.__run(("/bin/tar", "xfz",
                        "safir-sdk-core_" + version_string + ".orig.tar.gz"),
                       "extracting archive")
        os.chdir("safir-sdk-core_" + version_string)
        if not self.noclean:
            shutil.copytree(os.path.join("build", "packaging", "debian"), "debian")
        command = ["debuild",
                    "--prepend-path", os.path.dirname(which("conan"))]

        #set up some build options.
        options = "config=" + self.arguments.configs[0]
        if self.arguments.configs[0] == "Debug":
            options += " noopt"
        if self.arguments.skip_tests:
            options += " nocheck"
        command += ("--set-envvar", "DEB_BUILD_OPTIONS=" + options)

        #Pass along a few select environment variables to debuild, since by default no environment
        #is passed over to the build process by debuild.
        for var in ("SAFIR_SKIP_SLOW_TESTS",):
            val = os.environ.get(var)
            if val is not None:
                command += ("--set-envvar", var + "=" + val)

        #Inject the git revision so debian/rules can pass it on to cmake.
        for key, val in (git_info or {}).items():
            command += ("--set-envvar", key + "=" + val)

        command += ["-us", "-uc", "-nc"]

        self.__run(command,
                   "building packages")
        os.chdir(glob.glob("obj-*")[0])
        if not self.arguments.skip_tests:
            translate_results_to_junit("debhelper")

def getText(nodelist):
    """Get text data out of a xml dom"""
    rc = []
    for node in nodelist:
        if node.nodeType == node.TEXT_NODE:
            rc.append(node.data)
    return ''.join(rc)


def translate_results_to_junit(suite_name):
    """Translate ctest output to junit output"""
    with open(os.path.join("Testing", "TAG"), 'rb') as tag_file:
        dirname = tag_file.readline().decode("utf-8").strip()
    dom = xml.dom.minidom.parse(os.path.join("Testing", dirname, "Test.xml"))

    # Buffer the testcases so the <testsuite> open tag can carry aggregate
    # attributes (tests/failures/time). Without at least one attribute on
    # <testsuite>, xml2js (used by dorny/test-reporter) produces no attribute
    # object and the java-junit parser crashes on testsuite.$.time.
    testcases = []
    total_time = 0.0
    num_failures = 0

    testing = dom.getElementsByTagName("Testing")[0]
    for child in testing.childNodes:
        if child.nodeType == xml.dom.Node.ELEMENT_NODE:
            if child.tagName == "Test":
                test_name = getText(child.getElementsByTagName("Name")[0].childNodes)
                #test_target = os.path.split(getText(child.getElementsByTagName("Path")[0].childNodes))[-1]
                test_status = child.getAttribute("Status")
                for meas in child.getElementsByTagName("NamedMeasurement"):
                    if meas.getAttribute("name") == "Exit Code":
                        exit_code = getText(meas.getElementsByTagName("Value")[0].childNodes)
                    if meas.getAttribute("name") == "Exit Value":
                        exit_value = getText(meas.getElementsByTagName("Value")[0].childNodes)
                    if meas.getAttribute("name") == "Execution Time":
                        execution_time = float(getText(meas.getElementsByTagName("Value")[0].childNodes))

                meas = child.getElementsByTagName("Measurement")[0]

                total_time += execution_time
                case = ("  <testcase name=\"" + test_name + "\" classname=\"" + suite_name + "\" time=\"" +
                        str(execution_time) + "\">\n")
                output = escape(getText(meas.getElementsByTagName("Value")[0].childNodes))
                if test_status == "passed":
                    #success
                    case += "<system-out>" + output + "\n</system-out>\n"
                else:
                    #failure
                    num_failures += 1
                    case += ("<error message=\"" + exit_code + "(" + exit_value + ")\">" + output +
                             "\n</error>\n")
                case += "  </testcase>\n"
                testcases.append(case)

    with open(suite_name + ".junit.xml", "w") as junitfile:
        junitfile.write("<?xml version=\"1.0\"?>\n")
        junitfile.write("<testsuite name=\"" + suite_name + "\" tests=\"" + str(len(testcases)) +
                        "\" failures=\"" + str(num_failures) + "\" time=\"" + str(total_time) + "\">\n")
        for case in testcases:
            junitfile.write(case)
        junitfile.write("</testsuite>")


def get_builder(arguments):
    """"Get the correct builder for platform and arguments"""
    if VisualStudioBuilder.can_use():
        return VisualStudioBuilder(arguments)
    elif arguments.package and DebianPackager.can_use():
        return DebianPackager(arguments)
    elif UnixGccBuilder.can_use():
        return UnixGccBuilder(arguments)
    else:
        die("Failed to work out what builder to use!")


def execute(arguments):
    """Run a build for the given (already finalized) arguments and return a process
    exit code. This is the shared tail of both entry points."""
    #reduce process priority (currently only done on unix platforms)
    if hasattr(os, "nice"):
        try:
            if os.nice(0) == 0:
                os.nice(10)
        except OSError as exception:
            LOGGER.log("Failed to set process niceness: " + str(exception))

    try:
        builder = get_builder(arguments)
        builder.build()
        tests, failed = builder.total_tests, builder.failed_tests
        LOGGER.log("Result", "header")
        LOGGER.log("Operation completed successfully!")
        if tests == -1:
            pass
        elif tests == 0:
            LOGGER.log("No tests were performed.")
        elif failed == 0:
            LOGGER.log("All tests ran successfully!")
        else:
            LOGGER.log(str(failed) + " tests failed out of " + str(tests) + ".", "brief")
        result = 0
    except FatalError as exception:
        LOGGER.log("Result", "header")
        LOGGER.log("Build script failed:")
        LOGGER.log(str(exception), "output")
        LOGGER.log(str(exception), "brief")
        result = 1

    LOGGER.close()
    return result
