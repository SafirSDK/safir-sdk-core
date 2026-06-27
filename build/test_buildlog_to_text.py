#!/usr/bin/env python3
# -*- coding: utf-8 -*-
###############################################################################
#
# Copyright Saab AB, 2026 (http://safirsdkcore.com)
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
"""Unit tests for build/buildlog_to_text.py."""
# pylint: disable=missing-class-docstring,missing-function-docstring
import unittest

import buildlog_to_text

CHECKOUT = "/home/runner/work/safir-sdk-core/safir-sdk-core"


def _pre(body):
    """Wrap text in a plain output <pre> block, as build.py's Logger does."""
    return "<pre>" + body + "</pre>"


class SanitizeTest(unittest.TestCase):  # pylint: disable=too-many-public-methods

    def test_conan_dependency_warning_dropped(self):
        log = _pre("/home/runner/.conan2/p/b/abc/b/src/absl/strings/str_cat.cc:51:11: "
                   "warning: writing into a region [-Wstringop-overflow=]")
        self.assertEqual(buildlog_to_text.sanitize(log), "")

    def test_windows_conan_dependency_warning_dropped(self):
        log = _pre(r"D:\.conan2\p\b\boost\b\boost\type.hpp(10): warning C4244: conversion")
        self.assertEqual(buildlog_to_text.sanitize(log), "")

    def test_system_header_warning_dropped(self):
        log = _pre("/usr/include/foo.h:3:1: warning: something from a system header")
        self.assertEqual(buildlog_to_text.sanitize(log), "")

    def test_our_code_absolute_warning_kept_and_relativised(self):
        log = _pre(CHECKOUT + "/src/dose/dose_main.cpp:42:9: "
                   "warning: unused variable 'x' [-Wunused-variable]")
        out = buildlog_to_text.sanitize(log)
        self.assertIn("src/dose/dose_main.cpp:42:9: warning: unused variable", out)
        self.assertNotIn(CHECKOUT, out)
        self.assertFalse(out.startswith("/"))

    def test_deb_extraction_path_relativised(self):
        log = _pre(CHECKOUT + "/tmp/safir-sdk-core_7.4.3~alpha3/src/lluf/util.cpp:7:1: "
                   "warning: deprecated conversion")
        out = buildlog_to_text.sanitize(log)
        self.assertTrue(out.startswith("src/lluf/util.cpp:7:1: warning:"), out)

    def test_relative_path_warning_kept(self):
        log = _pre("src/dots/types.cpp:5:2: warning: shadowed declaration")
        out = buildlog_to_text.sanitize(log)
        self.assertIn("src/dots/types.cpp:5:2: warning: shadowed declaration", out)

    def test_green_command_echo_ignored(self):
        # The command echo lives in a <pre style="color: green"> block and may
        # contain .conan2 paths and -W flags; it must never produce a finding.
        log = ('<pre style="color: green">g++ -Wall -I/home/runner/.conan2/include '
               'src/foo.cpp</pre>')
        self.assertEqual(buildlog_to_text.sanitize(log), "")

    def test_html_entities_unescaped(self):
        log = _pre("src/foo.cpp:1:1: warning: comparison of &lt;int&gt; &amp; mask")
        out = buildlog_to_text.sanitize(log)
        self.assertIn("comparison of <int> & mask", out)

    def test_lintian_and_dh_lines_passed_through(self):
        # These carry no checkout path; they must survive for the lintian
        # converter (and so dh_* warnings stay visible too).
        log = _pre("W: safir-sdk-core-dev: no-manual-page [usr/bin/safir_build_common.py]\n"
                   "dh_cligacpolicy: warning: No Build-Depends on cli-common-dev!")
        out = buildlog_to_text.sanitize(log)
        self.assertIn("W: safir-sdk-core-dev: no-manual-page", out)
        self.assertIn("dh_cligacpolicy: warning:", out)

    def test_ctest_custom_cmake_warning_dropped(self):
        # Jenkins excluded CTestCustom.cmake explicitly; this fires every build.
        log = _pre("CMake Warning at " + CHECKOUT + "/tmp/safir-sdk-core_7.4.3~alpha3/obj-x86_64-linux-gnu/"
                   "CTestCustom.cmake:3 (MESSAGE):")
        self.assertEqual(buildlog_to_text.sanitize(log), "")

    def test_cmake_warning_kept_and_relativised(self):
        log = _pre("CMake Warning at " + CHECKOUT + "/src/dose/CMakeLists.txt:12 (message):")
        out = buildlog_to_text.sanitize(log)
        self.assertIn("CMake Warning at src/dose/CMakeLists.txt:12 (message):", out)
        self.assertNotIn(CHECKOUT, out)

    def test_cmake_warning_without_location_kept(self):
        log = _pre("CMake Warning:")
        self.assertEqual(buildlog_to_text.sanitize(log).strip(), "CMake Warning:")

    def test_alink_warning_dropped(self):
        # Cosmetic .NET assembly-linker resource-path warnings are not reported.
        log = _pre("ALINK: warning A99999: Path 'Safir.Dob.dll.policy' in the resource name "
                   "is not supported. Using just file name 'Safir.Dob.dll.policy'")
        self.assertEqual(buildlog_to_text.sanitize(log), "")

    def test_noise_cmake_conandeps_block_dropped(self):
        # The "CMake Warning ...:" header is what analysis-model keys on; dropping
        # it removes the finding. The indented body lines are harmless plain text.
        log = _pre("CMake Warning at src/cmake/conan_provider.cmake:595 (message):\n"
                   "  Cmake-conan: CMakeConfigDeps generator was not defined in the conanfile\n"
                   "Call Stack (most recent call first):\n"
                   "  CMakeLists.txt:50 (find_package)")
        out = buildlog_to_text.sanitize(log)
        self.assertNotIn("CMake Warning", out)

    def test_noise_cmake_manually_specified_block_dropped(self):
        log = _pre("CMake Warning:\n"
                   "  Manually-specified variables were not used by the project:\n"
                   "    SAFIR_GIT_BRANCH")
        out = buildlog_to_text.sanitize(log)
        self.assertNotIn("CMake Warning", out)

    def test_noise_cmake_findqt_block_dropped(self):
        log = _pre("CMake Warning at vehiclemmi/src/cpp/CMakeLists.txt:10 (find_package):\n"
                   '  By not providing "FindQt6.cmake" in CMAKE_MODULE_PATH this project has')
        out = buildlog_to_text.sanitize(log)
        self.assertNotIn("CMake Warning", out)

    def test_real_cmake_warning_survives_noise_filter(self):
        # A genuine message warning with a blank line before its body must stay.
        log = _pre("CMake Warning at " + CHECKOUT + "/CMakeLists.txt:167 (message):\n"
                   "  Failed to obtain git revision.  Version will not include git hash.")
        out = buildlog_to_text.sanitize(log)
        self.assertIn("CMake Warning at CMakeLists.txt:167 (message):", out)
        self.assertIn("Failed to obtain git revision", out)

    def test_mixed_block_keeps_only_our_warnings(self):
        log = _pre("/home/runner/.conan2/p/b/x/str_cat.cc:51:11: warning: dep warning [-Wfoo]\n" + CHECKOUT +
                   "/src/ours.cpp:9:1: warning: our warning [-Wbar]\n"
                   "/home/runner/.conan2/p/b/x/str_cat.cc:51:11: warning: dep warning [-Wfoo]")
        out = buildlog_to_text.sanitize(log)
        self.assertEqual(out.strip(), "src/ours.cpp:9:1: warning: our warning [-Wbar]")

    def test_conan_section_dependency_warnings_stripped(self):
        # Dependency cmake/boost warnings have relative paths just like ours, but
        # sit inside the conan-install window and must be dropped; our warning
        # before the window and after the "Finalizing install" marker survive.
        log = _pre("CMake Warning at CMakeLists.txt:167 (message):\n"
                   "-- CMake-Conan: conan install /x -of=/y --build=missing\n"
                   "CMake Warning at CMakeLists.txt:131 (FIND_PACKAGE):\n"
                   "warning: in main-target boost_cobalt at libs/cobalt/build/Jamfile:73\n"
                   "======== Finalizing install (deploy, generators) ========\n"
                   "CMake Warning at src/lluf/CMakeLists.txt:49 (message):")
        out = buildlog_to_text.sanitize(log)
        self.assertIn("CMake Warning at CMakeLists.txt:167 (message):", out)
        self.assertIn("CMake Warning at src/lluf/CMakeLists.txt:49 (message):", out)
        self.assertNotIn("CMakeLists.txt:131", out)
        self.assertNotIn("boost_cobalt", out)

    def test_multiple_conan_sections_stripped(self):
        # Windows opens one window per config (Debug + RelWithDebInfo).
        log = _pre("-- CMake-Conan: conan install A\n"
                   "CMake Warning at CMakeLists.txt:1 (cmake_minimum_required):\n"
                   "======== Finalizing install (deploy, generators) ========\n"
                   "warning: Tag 'DOT_MULTI_TARGETS' has become obsolete.\n"
                   "-- CMake-Conan: conan install B\n"
                   "CMake Warning (dev) at qtbase/cmake/Helpers.cmake:73 (foo):\n"
                   "======== Finalizing install (deploy, generators) ========")
        out = buildlog_to_text.sanitize(log)
        self.assertIn("DOT_MULTI_TARGETS", out)
        self.assertNotIn("cmake_minimum_required", out)
        self.assertNotIn("qtbase", out)

    def test_plain_text_log_without_html(self):
        # The examples/docs builds tee plain stdout (no <pre> wrapper); the same
        # conan filter must still apply line-by-line.
        log = ("/home/runner/.conan2/p/b/x/dep.cpp:1:1: warning: dep [-Wd]\n" + CHECKOUT +
               "/examples/foo.cpp:2:2: warning: ours [-Wo]")
        out = buildlog_to_text.sanitize(log)
        self.assertEqual(out.strip(), "examples/foo.cpp:2:2: warning: ours [-Wo]")

    def test_ctest_failure_output_stripped(self):
        # A failing Boost.Test case prints "main.cpp(70): error: ... has failed",
        # which the compiler parsers would flag. ctest output sits between
        # "Test project" and "Total Test time", and the .deb packaging warnings
        # that follow it must survive.
        log = _pre("CMake Warning at " + CHECKOUT + "/CMakeLists.txt:167 (message):\n"
                   "Test project /x/obj-x86_64-linux-gnu\n"
                   "    Start 41: Communication_ResetTest\n"
                   "../tests/reset_test/main.cpp(70): error: in \"unicast\": check "
                   "receiver.GetRecvCount(2) >= 200 has failed\n"
                   "Total Test time (real) = 166.04 sec\n"
                   "dpkg-shlibdeps: warning: diversions involved - output may be incorrect")
        out = buildlog_to_text.sanitize(log)
        self.assertIn("CMake Warning at CMakeLists.txt:167 (message):", out)
        self.assertIn("dpkg-shlibdeps: warning:", out)
        self.assertNotIn("reset_test", out)
        self.assertNotIn("has failed", out)


if __name__ == "__main__":
    unittest.main()
