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
"""Unit tests for build/packaging_to_sarif.py."""
# pylint: disable=missing-class-docstring,missing-function-docstring
import json
import os
import tempfile
import unittest

import packaging_to_sarif

# A real slice of the .deb packaging output: two lintian warnings, the benign
# dpkg-shlibdeps "diversions involved" line (suppressed as known noise), plus the
# dpkg/dh tooling warnings that appear during the package build. Of these five
# lines, four become findings - the diversions line is dropped.
REAL = ("W: safir-sdk-core-dev: no-manual-page [usr/bin/safir_build_common.py]\n"
        "W: safir-sdk-core-dev: script-with-language-extension [usr/bin/safir_build_common.py]\n"
        "dpkg-shlibdeps: warning: diversions involved - output may be incorrect\n"
        "dh_cligacpolicy: warning: Warning! No Build-Depends(-Indep) on cli-common-dev (>= 0.5.7)!\n"
        "dh_auto_configure: warning: Use of debian/compat is deprecated and will be removed in debhelper (>= 14~).\n")


class ParseLintianTest(unittest.TestCase):

    def test_warning_with_path_location(self):
        findings = packaging_to_sarif.parse_logs(
            ["W: safir-sdk-core-dev: no-manual-page [usr/bin/safir_build_common.py]"])
        self.assertEqual(len(findings), 1)
        finding = findings[0]
        self.assertEqual(finding["tool"], "lintian")
        self.assertEqual(finding["severity"], "warning")
        self.assertEqual(finding["rule"], "no-manual-page")
        self.assertEqual(finding["uri"], "usr/bin/safir_build_common.py")

    def test_error_severity(self):
        findings = packaging_to_sarif.parse_logs(["E: safir-sdk-core: some-tag boom"])
        self.assertEqual(findings[0]["severity"], "error")

    def test_no_path_falls_back_to_package(self):
        findings = packaging_to_sarif.parse_logs(["W: safir-sdk-core: dir-or-file-in-opt"])
        self.assertEqual(findings[0]["uri"], "safir-sdk-core")


class ParseDebhelperTest(unittest.TestCase):

    def test_dpkg_shlibdeps_real_warning_still_reported(self):
        # A genuine dpkg-shlibdeps warning must still surface; only the specific
        # benign "diversions involved" line is suppressed.
        findings = packaging_to_sarif.parse_logs(
            ["dpkg-shlibdeps: warning: package could be avoiding an unnecessary dependency"])
        self.assertEqual(len(findings), 1)
        self.assertEqual(findings[0]["tool"], "dpkg-shlibdeps")

    def test_diversions_warning_suppressed(self):
        # libc6 /usr-merge diversion noise (dpkg bug #1035904) - not our bug,
        # harmless, dropped so the report stays clean.
        findings = packaging_to_sarif.parse_logs([
            "dpkg-shlibdeps: warning: diversions involved - output may be incorrect\n"
            " diversion by libc6 from: /lib64/ld-linux-x86-64.so.2\n"
            "dpkg-shlibdeps: warning: diversions involved - output may be incorrect\n"
            " diversion by libc6 to: /lib64/ld-linux-x86-64.so.2.usr-is-merged\n"
        ])
        self.assertEqual(findings, [])

    def test_dh_warning(self):
        findings = packaging_to_sarif.parse_logs(
            ["dh_cligacpolicy: warning: No Build-Depends(-Indep) on cli-common-dev!"])
        self.assertEqual(findings[0]["tool"], "dh_cligacpolicy")

    def test_dh_auto_warning(self):
        findings = packaging_to_sarif.parse_logs(["dh_auto_configure: warning: Use of debian/compat is deprecated."])
        self.assertEqual(findings[0]["tool"], "dh_auto_configure")

    def test_command_echo_not_matched(self):
        # The command line that *invokes* dh_auto_configure is not a warning.
        findings = packaging_to_sarif.parse_logs(
            ["CONAN_HOME=/root/.conan2 dh_auto_configure -- -DCMAKE_BUILD_TYPE=RelWithDebInfo"])
        self.assertEqual(findings, [])


class ParseCommonTest(unittest.TestCase):

    def test_non_packaging_lines_ignored(self):
        findings = packaging_to_sarif.parse_logs([
            "src/foo.cpp:1:1: warning: unused\n"
            "Now running lintian safir-sdk-core_7.4.3~alpha3-1_amd64.changes ...\n"
            "[1/247] Building CXX object foo.o\n"
        ])
        self.assertEqual(findings, [])

    def test_dedupe_across_logs(self):
        # The same findings come from three Linux logs -> one of each (the
        # diversions line in REAL is suppressed, so four not five).
        findings = packaging_to_sarif.parse_logs([REAL, REAL, REAL])
        self.assertEqual(len(findings), 4)

    def test_distinct_files_not_deduped(self):
        findings = packaging_to_sarif.parse_logs(
            ["W: pkg: no-manual-page [usr/bin/a]\nW: pkg: no-manual-page [usr/bin/b]"])
        self.assertEqual(len(findings), 2)


class SarifTest(unittest.TestCase):

    def test_sarif_shape(self):
        sarif = packaging_to_sarif.to_sarif(packaging_to_sarif.parse_logs([REAL]))
        self.assertEqual(sarif["version"], "2.1.0")
        run = sarif["runs"][0]
        self.assertEqual(run["tool"]["driver"]["name"], "Debian packaging")
        self.assertEqual(len(run["results"]), 4)
        # Every result's ruleId must be declared in the driver's rule list.
        rule_ids = {rule["id"] for rule in run["tool"]["driver"]["rules"]}
        for result in run["results"]:
            self.assertIn(result["ruleId"], rule_ids)
        self.assertIn("no-manual-page", rule_ids)
        self.assertIn("dh_cligacpolicy", rule_ids)


class MainTest(unittest.TestCase):

    def test_missing_root_writes_empty_sarif_and_exits_zero(self):
        # A cancelled run / all-failed build means download-artifact created no
        # buildlogs dir; the job must still stay green with an empty report.
        with tempfile.TemporaryDirectory() as tmp:
            missing = os.path.join(tmp, "buildlogs")  # never created
            output = os.path.join(missing, "packaging.sarif")
            rc = packaging_to_sarif.main(["--root", missing, "-o", output])
            self.assertEqual(rc, 0)
            with open(output, encoding="utf-8") as handle:
                sarif = json.load(handle)
            self.assertEqual(sarif["runs"][0]["results"], [])


if __name__ == "__main__":
    unittest.main()
