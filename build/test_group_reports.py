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
"""Unit tests for build/group_reports.py.

The fixtures are real report shapes, trimmed. They are the point of these tests: the
grouper is only useful if it still recognises what the sanitizers and valgrind actually
print, and those formats change between toolchain versions. A failure here means a
format moved, not that the code got worse.
"""
# pylint: disable=missing-class-docstring,missing-function-docstring
import argparse
import unittest

import group_reports

SRC = "/home/lars/safir/safir-sdk-core/"
# Valgrind ends each report with a bare "==pid== " line. The trailing space is what
# terminates it, so it is spelled out here rather than left in a string literal where an
# editor could trim it away.
VG_END = "==4242== " + "\n"

# Two reports of one defect, differing only in address, read size and thread number -
# exactly the pair that has to collapse to a single signature for the tool to earn its
# keep. Frame #1 is in boost, so it must not reach the signature.
ASAN = """==1234==ERROR: AddressSanitizer: heap-use-after-free on address 0x603000000040 at pc 0x49d1f2
READ of size 4 at 0x603000000040 thread T0
    #0 0x49d1f1 in ProcessMonitorImpl::Poll(int) {src}src/lluf/lluf_internal.ss/src/ProcessMonitorLinux.cpp:88
    #1 0x7f1 in boost::asio::detail::executor_op::do_complete(void*) /usr/include/boost/asio/detail/executor_op.hpp:70
SUMMARY: AddressSanitizer: heap-use-after-free
==================
""".format(src=SRC)

ASAN_AGAIN = """==5678==ERROR: AddressSanitizer: heap-use-after-free on address 0x60300000aaaa at pc 0x49d1f2
READ of size 8 at 0x60300000aaaa thread T3
    #0 0x49d1f1 in ProcessMonitorImpl::Poll(int) {src}src/lluf/lluf_internal.ss/src/ProcessMonitorLinux.cpp:88
    #1 0x7f1 in boost::asio::detail::executor_op::do_complete(void*) /usr/include/boost/asio/detail/executor_op.hpp:70
SUMMARY: AddressSanitizer: heap-use-after-free
==================
""".format(src=SRC)

# UBSan carries its location on the report line itself and has no stack block.
UBSAN = ("{src}src/dots/dots_internal.ss/src/RepositoryLocal.h:120:9: runtime error: load of value "
         "3200171710, which is not a valid value for type 'DotsC_MemberType'\n").format(src=SRC)

# TSan puts the pid on the first line, which must not become part of the signature.
TSAN = """WARNING: ThreadSanitizer: data race (pid=9999)
  Write of size 8 at 0x7b0400000000 by thread T2:
    #0 Safir::Dob::Internal::Connections::Initialize(bool, long) {src}src/dose/dose_internal.ss/src/Connections.cpp:80
    #1 Safir::Dob::Internal::InitializeDoseInternalFromApp() {src}src/dose/dose_internal.ss/src/Initialize.cpp:99
SUMMARY: ThreadSanitizer: data race
""".format(src=SRC)

# Valgrind frames look nothing like a sanitizer's, and only carry full paths when it is
# run with --fullpath-after= .
MEMCHECK = """==4242== Conditional jump or move depends on uninitialised value(s)
==4242==    at 0x4A1B2C: GetHashedValue(int) const ({src}src/dots/dots_internal.ss/src/RepositoryLocal.h:255)
==4242==    by 0x4A2000: CopyToShm ({src}src/dots/dots_internal.ss/src/RepositoryToShm.cpp:60)
""".format(src=SRC) + VG_END

# Leaks are reported with a byte count and a loss record number, both of which differ
# between two reports of the same leak.
LEAK = """==4242== 24 bytes in 1 blocks are definitely lost in loss record 12 of 345
==4242==    at 0x483BE63: operator new(unsigned long) (vg_replace_malloc.c:472)
==4242==    by 0x4A2000: Safir::Dob::Internal::Leaky() ({src}src/dose/dose_internal.ss/src/Leaky.cpp:12)
""".format(src=SRC) + VG_END


def group(*texts, frames=2, src=SRC):
    """Run the grouper over the given report texts, as main() would over files."""
    args = argparse.Namespace(src=src, frames=frames)
    found = []
    for text in texts:
        found.extend(group_reports.reports_in(text.splitlines(), args))
    return found


class SignatureTest(unittest.TestCase):

    def test_same_defect_from_two_processes_is_one_signature(self):
        reports = group(ASAN, ASAN_AGAIN)
        self.assertEqual(len(reports), 2)
        first, second = reports
        self.assertEqual((first[0], first[1]), (second[0], second[1]))

    def test_address_and_size_are_not_part_of_the_kind(self):
        kind = group(ASAN)[0][0]
        self.assertNotIn("0x", kind)
        self.assertIn("heap-use-after-free", kind)

    def test_system_frames_are_not_ours(self):
        frames = group(ASAN)[0][1]
        self.assertEqual(len(frames), 1)
        self.assertIn("ProcessMonitorLinux.cpp:88", frames[0])

    def test_frame_limit_is_honoured(self):
        self.assertEqual(len(group(MEMCHECK, frames=1)[0][1]), 1)
        self.assertEqual(len(group(MEMCHECK, frames=2)[0][1]), 2)

    def test_paths_are_shown_relative_to_the_source_tree(self):
        self.assertTrue(group(ASAN)[0][1][0].endswith("src/lluf/lluf_internal.ss/src/ProcessMonitorLinux.cpp:88"))


class FormatTest(unittest.TestCase):

    def test_ubsan_takes_its_location_from_the_report_line(self):
        reports = group(UBSAN)
        self.assertEqual(len(reports), 1)
        kind, frames, _ = reports[0]
        self.assertIn("runtime error", kind)
        self.assertEqual(frames, ["src/dots/dots_internal.ss/src/RepositoryLocal.h:120:9"])

    def test_tsan_pid_is_stripped_from_the_kind(self):
        kind, frames, _ = group(TSAN)[0]
        self.assertEqual(kind, "WARNING: ThreadSanitizer: data race")
        self.assertEqual(len(frames), 2)

    def test_valgrind_frames_are_recognised(self):
        kind, frames, _ = group(MEMCHECK)[0]
        self.assertEqual(kind, "Conditional jump or move depends on uninitialised value(s)")
        self.assertIn("RepositoryLocal.h:255", frames[0])
        self.assertIn("RepositoryToShm.cpp:60", frames[1])

    def test_leak_size_and_loss_record_are_normalised_away(self):
        kind, frames, _ = group(LEAK)[0]
        self.assertEqual(kind, "definitely lost")
        self.assertIn("Leaky.cpp:12", frames[0])

    def test_report_at_the_end_of_a_truncated_log_is_not_lost(self):
        # A process killed part way through writing a report - a test that timed out, or
        # one cleaned up between tests. The report has no terminating line.
        truncated = MEMCHECK[:-len(VG_END)]
        self.assertEqual(len(group(truncated)), 1)

    def test_every_format_is_found_in_one_pass(self):
        kinds = {report[0] for report in group(ASAN, UBSAN, TSAN, MEMCHECK, LEAK)}
        self.assertEqual(len(kinds), 5)


class OursTest(unittest.TestCase):

    def test_src_prefix_decides_when_given(self):
        self.assertTrue(group_reports.is_ours(SRC + "src/dose/x.cpp", SRC))
        self.assertFalse(group_reports.is_ours("/usr/include/boost/asio.hpp", SRC))

    def test_without_src_system_and_conan_paths_are_excluded(self):
        self.assertTrue(group_reports.is_ours("/home/lars/safir/src/dose/x.cpp", None))
        self.assertFalse(group_reports.is_ours("/usr/include/boost/asio.hpp", None))
        self.assertFalse(group_reports.is_ours("/home/lars/.conan2/p/boost/include/asio.hpp", None))

    def test_short_path_falls_back_to_src_when_no_prefix_given(self):
        self.assertEqual(group_reports.short_path("/build/tree/src/dose/x.cpp", None), "src/dose/x.cpp")


if __name__ == "__main__":
    unittest.main()
