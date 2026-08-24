#!/usr/bin/env python3
# -*- coding: utf-8 -*-
###############################################################################
#
# Copyright Saab AB, 2026 (http://safirsdkcore.com)
#
# Created by: Lars Hagstrom (lars@foldspace.nu)
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
#
# Shared JUnit reporting for the installed slow-test suite.
#
# The slow, system-level tests were moved out of ctest into the installed
# TestSuite component (see src/tests/slow_test_suite). Unlike ctest, that path
# produced no machine-readable results, so a failing case was only visible as a
# red CI job plus a long console log. This helper lets each migrated driver emit
# a JUnit XML file - one <testcase> per named case - so the slow suite feeds the
# same consolidated "Test results" check as the dose/ctest suites do, with
# per-case granularity.
#
# It generalises the dose test's write_test_result (run_dose_tests.py.in): where
# dose writes one file per case with a hardcoded time="0", this collects every
# case of a driver into a single <testsuite> file with real per-case timing.
#
# Signalling model (matches run_dose_tests): a test-case *failure* is carried by
# the junit only - the driver still writes its file and the CI job stays green;
# the "Test results" check turns red from the junit. The job goes red only for a
# genuine infra failure (a driver that crashes or hangs before writing junit),
# which the umbrella (run_slow_tests) detects from a missing file / bad exit.
#
import os
import sys
import time
import traceback
from contextlib import contextmanager

try:
    from output import log
except ImportError:
    def log(*args):
        print(" ".join(str(a) for a in args))
        sys.stdout.flush()


def _escape_attr(text):
    """Escape a string for use in an XML attribute value."""
    return (str(text).replace("&", "&amp;").replace("<", "&lt;")
            .replace(">", "&gt;").replace('"', "&quot;"))


def _cdata(text):
    """Wrap text in a CDATA section. The only sequence CDATA can't hold verbatim
    is ']]>', which we split across two sections (same trick as run_dose_tests)."""
    return "<![CDATA[" + str(text).replace("]]>", "]]]]><![CDATA[>") + "]]>"


class JUnitReporter:
    """Accumulates the test-case results of one slow-test driver and writes them
    to a single <suite>.junit.xml file.

    output_dir defaults to the SAFIR_SLOW_TEST_JUNIT_DIR environment variable
    (set per-driver by the run_slow_tests umbrella) or, failing that, the current
    working directory - so a driver run directly by a developer just drops its
    junit next to where it runs. The directory is resolved to an absolute path up
    front so a later os.chdir() by the driver doesn't move the file."""

    def __init__(self, suite, classname=None, output_dir=None):
        self.suite = suite
        self.classname = classname or suite
        chosen = output_dir or os.environ.get("SAFIR_SLOW_TEST_JUNIT_DIR") or os.getcwd()
        self.output_dir = os.path.abspath(chosen)
        self.cases = []

    def add_case(self, name, passed, duration=0.0, failure_text=""):
        """Record one test case. failure_text is included (in a CDATA <failure>)
        only when passed is false."""
        self.cases.append({
            "name": name,
            "passed": bool(passed),
            "time": max(0.0, float(duration)),
            "failure": "" if passed else (failure_text or "Failed"),
        })
        log("Test", self.suite + "." + name, "success:", bool(passed))
        if not passed:
            log(" -", name, "failed!")
        return passed

    @contextmanager
    def test_case(self, name):
        """Context manager that times the block and records it as one case. An
        exception (including a deep sys.exit with a non-zero code) is caught,
        recorded as a failure with its traceback, and swallowed so the driver can
        carry on to the next case."""
        start = time.time()
        try:
            yield
        except SystemExit as e:
            code = e.code
            passed = code is None or code == 0
            self.add_case(name, passed, time.time() - start,
                          "" if passed else "exited with code %s" % (code,))
        except BaseException:
            self.add_case(name, False, time.time() - start, traceback.format_exc())
        else:
            self.add_case(name, True, time.time() - start)

    def run_single(self, name, func, *args, **kwargs):
        """Run a whole driver body as a single case and write the junit.

        For the monolithic drivers whose entire run is one test: func's return
        value is interpreted as a process-style result (0/None/True = pass,
        anything else = fail); an exception or a non-zero sys.exit is a failure.
        Returns the process exit code to hand to sys.exit()."""
        start = time.time()
        try:
            result = func(*args, **kwargs)
        except SystemExit as e:
            code = e.code if isinstance(e.code, int) else (0 if e.code is None else 1)
            self.add_case(name, code == 0, time.time() - start,
                          "" if code == 0 else "exited with code %d" % code)
            self.write()
            return code
        except BaseException:
            self.add_case(name, False, time.time() - start, traceback.format_exc())
            self.write()
            return 1
        #Interpret the return the way a process exit code would be: None (no
        #explicit result) or an integer 0 is success; a non-zero integer is
        #failure. Booleans are honoured too. Note we must not write this as
        #`result in (0, None, True)` - Python treats 1 == True, so a failing
        #return of 1 would wrongly count as a pass.
        if result is None or result is True:
            passed = True
        elif result is False:
            passed = False
        elif isinstance(result, int):
            passed = (result == 0)
        else:
            passed = bool(result)
        self.add_case(name, passed, time.time() - start,
                      "" if passed else "test reported failure (see log output)")
        self.write()
        return 0 if passed else 1

    @property
    def num_run(self):
        return len(self.cases)

    @property
    def num_failed(self):
        return sum(1 for c in self.cases if not c["passed"])

    def write(self):
        """Write all accumulated cases to <output_dir>/<suite>.junit.xml."""
        os.makedirs(self.output_dir, exist_ok=True)
        path = os.path.join(self.output_dir, self.suite + ".junit.xml")
        total = len(self.cases)
        failures = self.num_failed
        total_time = sum(c["time"] for c in self.cases)
        with open(path, "w") as f:
            f.write('<?xml version="1.0"?>\n')
            f.write('<testsuite name="%s" tests="%d" failures="%d" time="%.3f">\n'
                    % (_escape_attr(self.suite), total, failures, total_time))
            for c in self.cases:
                f.write('  <testcase name="%s" classname="%s" time="%.3f"'
                        % (_escape_attr(c["name"]), _escape_attr(self.classname), c["time"]))
                if c["passed"]:
                    f.write("/>\n")
                else:
                    f.write(">\n    <failure message=\"Failed\">"
                            + _cdata(c["failure"]) + "</failure>\n  </testcase>\n")
            f.write("</testsuite>\n")
        log("Wrote junit report", path, "(%d cases, %d failed)" % (total, failures))
        return path
