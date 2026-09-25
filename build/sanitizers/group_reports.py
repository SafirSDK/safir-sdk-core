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
"""Group ASan, UBSan, LSan, TSan and valgrind memcheck reports by signature.

A signature is the report kind plus the first --frames stack frames that are in our
own code, which is what tells two reports apart in practice: the same race or
uninitialised read reported from a thousand processes is one line here. The paths
given can be report files (log_path output, valgrind --log-file output, test output
that has reports mixed in) or directories, which are searched recursively.

Prints one line per signature with its count and the files it came from, most
frequent first, then one full example report for each of the first --show signatures.

A frame is "ours" if its source path is under --src, or, without --src, if it is not
under a system or conan directory. Valgrind reports need --fullpath-after= (as set by
valgrind_ctest.py) for their frames to carry full paths.
"""
import argparse
import collections
import os
import re

START = re.compile(r"(WARNING: ThreadSanitizer: .*?)(?: \(pid=\d+\))?$|(ERROR: AddressSanitizer: \S+)|"
                   r"(ERROR: LeakSanitizer: .*)|(runtime error: .*)|"
                   r"==\d+== ((?:Invalid|Conditional|Use of uninit|Syscall param|Mismatched|Source and dest|"
                   r"Argument .* of function|\d[\d,]* bytes in [\d,]* blocks are definitely).*)")
END = re.compile(r"^(SUMMARY:|==================$)|^==\d+== $")
# asan: "#0 0xaddr in func /path:line", tsan: "#0 func /path:line (lib+off)",
# valgrind (with --fullpath-after=): "at 0xADDR: func (/path:line)"
FRAME = re.compile(r"(?:#\d+ (?:0x[0-9a-f]+ in )?|(?:at|by) 0x[0-9A-F]+: )(.*?) \(?(/\S+?:\d+)")
SYSTEM_PREFIXES = ("/usr/", "/lib/", "/lib64/", "/opt/")
MAX_REPORT_LINES = 120


def is_ours(path, src):
    """True if a frame with this source path is in our code."""
    if src:
        return path.startswith(src)
    return not path.startswith(SYSTEM_PREFIXES) and "/.conan2/" not in path


def short_path(path, src):
    """The path relative to the source tree, for display."""
    if src and path.startswith(src):
        return path[len(src):]
    index = path.find("/src/")
    return path[index + 1:] if index >= 0 else path


def input_files(paths):
    """Yield the given files, and every file under the given directories."""
    for path in paths:
        if os.path.isdir(path):
            for root, _, names in os.walk(path):
                for name in names:
                    yield os.path.join(root, name)
        else:
            yield path


def reports_in(lines, args):
    """Yield (kind, frames, text) for every report found in lines."""
    cur = None
    for line in lines + [""]:
        match = START.search(line)
        if match and (cur is None or "runtime error" in line):
            if cur:
                yield cur
            kind = [g for g in match.groups() if g][0]
            # Drop what differs between reports of the same defect: addresses, sizes and
            # valgrind's "in loss record 12 of 345".
            kind = re.sub(r"[\d,]+ bytes in [\d,]+ blocks are definitely lost.*", "definitely lost", kind)
            kind = re.sub(r"0x[0-9a-f]+|\b\d+ bytes?\b", "N", kind)
            cur = (kind, [], [line])
            if "runtime error" in line:
                # UBSan puts the location on the report line itself.
                cur[1].append(short_path(line.split(": runtime error")[0], args.src))
                yield cur
                cur = None
        elif cur:
            cur[2].append(line)
            frame = FRAME.search(line)
            if frame and is_ours(frame.group(2), args.src) and len(cur[1]) < args.frames:
                cur[1].append(f"{frame.group(1)[:80]} @ {short_path(frame.group(2), args.src)}")
            if END.search(line) or len(cur[2]) > MAX_REPORT_LINES:
                yield cur
                cur = None


def main():
    """Parse the arguments, read the reports and print the signatures."""
    parser = argparse.ArgumentParser(description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter)
    parser.add_argument("paths", nargs="+", help="report files or directories")
    parser.add_argument("--frames", type=int, default=2, help="how many of our frames make up a signature")
    parser.add_argument("--show", type=int, default=0, help="print a full example of the first N signatures")
    parser.add_argument("--src", default=None, help="source tree prefix that marks a frame as ours")
    args = parser.parse_args()
    if args.src:
        args.src = os.path.abspath(args.src) + "/"

    signatures = collections.OrderedDict()
    for path in input_files(args.paths):
        try:
            with open(path, errors="replace", encoding="utf-8") as report_file:
                lines = report_file.read().splitlines()
        except OSError:
            continue
        for kind, frames, text in reports_in(lines, args):
            sig = signatures.setdefault((kind, tuple(frames)), {"count": 0, "files": set(), "text": text})
            sig["count"] += 1
            sig["files"].add(os.path.basename(path))

    print_signatures(signatures, args.show)


def print_signatures(signatures, show):
    """Print one line per signature, most frequent first, then the first examples."""
    ranked = sorted(signatures.items(), key=lambda item: -item[1]["count"])
    for i, ((kind, frames), sig) in enumerate(ranked):
        files = sorted(sig["files"])
        more = f" ..+{len(files) - 4}" if len(files) > 4 else ""
        frame_lines = "\n     ".join(frames) or "(no frame in our code)"
        print(f"[{i}] x{sig['count']} {kind}\n     {frame_lines}\n     files: {', '.join(files[:4])}{more}")
    for i, (_, sig) in enumerate(ranked[:show]):
        example = "\n".join(sig["text"][:80])
        print(f"\n======== example [{i}]\n{example}")


if __name__ == "__main__":
    main()
