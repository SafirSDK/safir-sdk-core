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
"""Exit non-zero if any junit report below a directory records a failure.

This deliberately contradicts the signalling rule the rest of ci.yml follows,
so it is only used by the Debug jobs. Everywhere else a test failure reddens
the aggregated "Test results" check while the job itself stays green, on the
grounds that a job answers "did it build and run?" and the check answers "did
the tests pass?".

The Debug jobs opt out because what fails there is different in kind. They are
the only builds where assert(), LeveledLock's lock-ORDER checking and
_GLIBCXX_ASSERTIONS are compiled in at all - every other build we produce or
ship has them preprocessed away. So a failure here is not "a test regressed",
it is "a class of bug that nothing else in CI can observe just fired", and it
should stop the line rather than tint a check that is easy to live with. #616
sat undetected for three years, and the most likely explanation is exactly
that: a red Debug row nobody was obliged to act on.

None of the test runners can do this themselves. debuild runs ctest behind a
leading "-" in debian/rules, run_dose_tests returns 0 on testcase diffs, and
run_slow_tests' testcase failures are deliberately not raised by run_test.py.
The junit they leave behind is the only place the result survives.
"""
import argparse
import glob
import os
import sys
import xml.etree.ElementTree as ET


def main(argv=None):
    """Run the check on argv (sys.argv[1:] when None) and return the exit code."""
    parser = argparse.ArgumentParser(description=__doc__,
                                     formatter_class=argparse.RawDescriptionHelpFormatter)
    parser.add_argument("directory",
                        nargs="?",
                        default=".",
                        help="Directory to search recursively for *.junit.xml (default: cwd)")
    parser.add_argument("--allow-empty",
                        action="store_true",
                        help="Treat 'no junit found' as success instead of failure.")
    arguments = parser.parse_args(argv)

    pattern = os.path.join(arguments.directory, "**", "*.junit.xml")
    reports = sorted(glob.glob(pattern, recursive=True))

    if not reports:
        if arguments.allow_empty:
            print("No junit reports found, but --allow-empty was given.")
            return 0
        #Producing nothing is itself a failure: it means the suite never ran, or
        #died before it could report, which is not something to pass silently.
        print("No junit reports found under '" + arguments.directory +
              "': the test run reported nothing at all.")
        return 1

    total = 0
    failures = []
    for report in reports:
        try:
            root = ET.parse(report).getroot()
        except ET.ParseError as exc:
            #A truncated report usually means the process died mid-write, which
            #is a failure in its own right rather than something to skip over.
            failures.append((os.path.relpath(report, arguments.directory), "unparsable: " + str(exc)))
            continue

        for case in root.iter("testcase"):
            total += 1
            for child in case:
                if child.tag in ("failure", "error"):
                    name = case.get("name") or "<unnamed>"
                    classname = case.get("classname")
                    if classname:
                        name = classname + "." + name
                    failures.append((name, child.tag))

    print(str(total) + " testcases in " + str(len(reports)) + " junit reports, " +
          str(len(failures)) + " failed")
    for name, kind in failures:
        print("  FAILED (" + kind + "): " + name)

    return 1 if failures else 0


if __name__ == "__main__":
    sys.exit(main())
