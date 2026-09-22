#!/usr/bin/env python3
# -*- coding: utf-8 -*-
###############################################################################
#
# Copyright Saab AB, 2026 (http://safirsdkcore.com)
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
"""Unit tests for build/ci/check_junit.py.

Lives next to the other CI-tooling tests in build/ (not in build/ci/) because
the warnings-summary job runs `unittest discover -s build`, which does not
descend into directories that are not packages.
"""
# pylint: disable=missing-class-docstring,missing-function-docstring
import contextlib
import io
import os
import sys
import tempfile
import unittest

sys.path.insert(0, os.path.join(os.path.dirname(os.path.abspath(__file__)), "ci"))
import check_junit  # pylint: disable=wrong-import-position

# The three shapes the junit producers in this tree actually emit: ctest via
# translate_results_to_junit() marks a failed case with <error>, run_dose_tests
# and the slow-suite junit.py mark it with <failure>. A passing case carries
# only <system-out> or is self-closing.
PASSING = ('<?xml version="1.0"?>\n'
           '<testsuite name="s" tests="2" failures="0" time="1.0">\n'
           '  <testcase name="one" classname="s" time="0.5"><system-out>ok</system-out></testcase>\n'
           '  <testcase name="two" classname="s" time="0.5"/>\n'
           '</testsuite>\n')

WITH_FAILURE = ('<?xml version="1.0"?>\n'
                '<testsuite name="dose_test" tests="1" failures="1" time="0">\n'
                '  <testcase name="008-complex_message" classname="dose_test" time="0">\n'
                '    <failure message="Failed"><![CDATA[diff here]]></failure>\n'
                '  </testcase>\n'
                '</testsuite>\n')

WITH_ERROR = ('<?xml version="1.0"?>\n'
              '<testsuite name="Debug" tests="1" failures="1" time="2.0">\n'
              '  <testcase name="leveled_lock_test" classname="Debug" time="2.0">\n'
              '<error message="1(Failed)">assertion failed\n</error>\n'
              '  </testcase>\n'
              '</testsuite>')

WITH_SKIPPED = ('<?xml version="1.0"?>\n'
                '<testsuite name="s" tests="1" failures="0" time="0">\n'
                '  <testcase name="not_here" classname="s" time="0"><skipped/></testcase>\n'
                '</testsuite>\n')


def run(*argv):
    """Run check_junit.main with argv, returning (exit code, captured stdout)."""
    out = io.StringIO()
    with contextlib.redirect_stdout(out):
        code = check_junit.main(list(argv))
    return code, out.getvalue()


class CheckJunitTest(unittest.TestCase):

    def setUp(self):
        self.tmp = tempfile.TemporaryDirectory()  # pylint: disable=consider-using-with
        self.addCleanup(self.tmp.cleanup)

    def write(self, relpath, content):
        path = os.path.join(self.tmp.name, relpath)
        os.makedirs(os.path.dirname(path), exist_ok=True)
        with open(path, "w") as f:
            f.write(content)

    def test_all_passing_succeeds(self):
        self.write("a.junit.xml", PASSING)
        code, out = run(self.tmp.name)
        self.assertEqual(code, 0)
        self.assertIn("2 testcases in 1 junit reports, 0 failed", out)

    def test_failure_element_fails_and_is_named(self):
        self.write("dose_test_output/008.junit.xml", WITH_FAILURE)
        code, out = run(self.tmp.name)
        self.assertEqual(code, 1)
        self.assertIn("FAILED (failure): dose_test.008-complex_message", out)

    def test_error_element_fails(self):
        self.write("Debug.junit.xml", WITH_ERROR)
        code, out = run(self.tmp.name)
        self.assertEqual(code, 1)
        self.assertIn("FAILED (error): Debug.leveled_lock_test", out)

    def test_skipped_is_not_a_failure(self):
        self.write("a.junit.xml", WITH_SKIPPED)
        code, _ = run(self.tmp.name)
        self.assertEqual(code, 0)

    def test_one_failure_among_many_reports_fails(self):
        #Reports are found recursively, and a single failing case anywhere is
        #enough - this is the debug-dose-tests shape: hundreds of one-case files.
        for i in range(20):
            self.write("dose_test_output/%03d-ok.junit.xml" % i, PASSING)
        self.write("dose_test_output/deep/nested/bad.junit.xml", WITH_FAILURE)
        code, out = run(self.tmp.name)
        self.assertEqual(code, 1)
        self.assertIn("41 testcases in 21 junit reports, 1 failed", out)

    def test_unparsable_report_fails(self):
        self.write("truncated.junit.xml", PASSING[:60])
        code, out = run(self.tmp.name)
        self.assertEqual(code, 1)
        self.assertIn("truncated.junit.xml", out)
        self.assertIn("unparsable", out)

    def test_no_reports_fails_by_default(self):
        code, out = run(self.tmp.name)
        self.assertEqual(code, 1)
        self.assertIn("No junit reports found", out)

    def test_no_reports_passes_with_allow_empty(self):
        code, _ = run(self.tmp.name, "--allow-empty")
        self.assertEqual(code, 0)

    def test_files_not_named_junit_xml_are_ignored(self):
        self.write("Test.xml", WITH_FAILURE)
        self.write("a.junit.xml", PASSING)
        code, _ = run(self.tmp.name)
        self.assertEqual(code, 0)


if __name__ == "__main__":
    unittest.main()
