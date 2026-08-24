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
"""Turn a buildlog.html (produced by build.py's Logger) into a clean plain-text
log that the Quality Monitor / analysis-model parsers can read.

This is a log *sanitizer*, not a warning parser - the downstream tool owns the
actual warning detection. Its two jobs are:

1. Unwrap the HTML: keep the text inside the plain ``<pre>`` output blocks and
   drop the ``<pre style="color: green">`` command-echo blocks (those contain
   the literal command lines, including dependency paths and ``-W`` flags that
   would otherwise be misread as warnings).

2. Separate "our code" from Conan dependency builds. Conan builds its
   dependencies from source (``--build=missing``) and their compiler warnings
   land in the same log. We keep only compiler diagnostics that belong to our
   own tree: a diagnostic carrying an *absolute* path is kept only when that
   path is under the checkout, so dependency builds (``~/.conan2/...``) and
   system headers (``/usr/include/...``) are dropped wherever the Conan cache
   happens to live. Absolute checkout paths are rewritten repo-relative so the
   downstream tool can link them to source after a fresh checkout. Lines that
   are not compiler diagnostics (lintian ``W:``/``E:``, ``dh_*`` warnings, plain
   output) are passed through verbatim.
"""
import argparse
import glob
import html
import os
import re
import sys

# The repository is checked out at <something>/safir-sdk-core/safir-sdk-core on
# the CI runners, and the .deb build re-extracts the sources under a
# tmp/safir-sdk-core_<version>/ subdirectory. Stripping this prefix turns an
# absolute build-time path into one that resolves against a plain checkout.
# Matches the leading (non-space) path token up to and including the checkout
# marker, so it works whether the path starts the line (gcc) or follows a
# prefix like "CMake Warning at " (cmake).
_CHECKOUT_PREFIX_RE = re.compile(r"\S*/safir-sdk-core/safir-sdk-core/(?:tmp/safir-sdk-core_[^/]*/)?")

# A path token is "absolute" if it starts with a POSIX root or a Windows drive.
_ABSOLUTE_PATH_RE = re.compile(r"^(?:/|[A-Za-z]:/)")

# GCC / Doxygen / javac diagnostics: "<path>:<line>[:<col>]: warning|error|note: ...".
_GCC_RE = re.compile(r"^(?P<path>(?:[A-Za-z]:)?[^:]+):\d+:(?:\d+:)?\s+(?:warning|error|note|fatal error)\b")

# MSVC / MSBuild diagnostics: "<path>(<line>[,<col>]): warning|error C####: ...".
_MSVC_RE = re.compile(r"^(?P<path>(?:[A-Za-z]:)?[^()]+)\(\d+(?:,\d+)?\):\s+(?:warning|error|fatal error)\s+[A-Z]+\d+")

# CMake diagnostics: "CMake Warning|Error [(dev)] [at <path>:<line> (<func>)]:".
_CMAKE_RE = re.compile(r"^CMake (?:Warning|Error)(?: \(dev\))?(?:\s+at\s+(?P<path>.+?):\d+\s+\([^)]*\))?:")

# The .NET assembly linker warns once per generated assembly that a path in the
# resource name is dropped. These are cosmetic and deliberately not reported.
_ALINK_RE = re.compile(r"^ALINK: warning ")

# A CMake warning is rendered as a "CMake Warning ...:" header followed by an
# indented body; analysis-model attaches the body to the header, so dropping the
# header line drops the whole finding. We suppress a few headers whose body's
# first content line is known dependency/infrastructure noise rather than a
# defect in our code:
#   * cmake-conan announcing it did not generate CMakeConfigDeps,
#   * CMake reporting build variables our top-level project forwards but a
#     sub-build does not consume,
#   * the standard "tried module mode, falling back to config mode" notice for
#     Qt (emitted by our vehiclemmi CMakeLists, but not an actionable problem).
_NOISE_CMAKE_BODY_MARKERS = (
    "Cmake-conan: CMakeConfigDeps generator was not defined",
    "Manually-specified variables were not used by the project",
    'By not providing "FindQt',
)

# Extract every ``<pre ...>...</pre>`` block; the attribute text distinguishes
# the green command echoes from the plain output blocks we want.
_PRE_BLOCK_RE = re.compile(r"<pre(?P<attrs>[^>]*)>(?P<body>.*?)</pre>", re.DOTALL)

# Conan builds its dependencies from source between these two markers. The
# dependency CMake/Boost/meson warnings emitted there have RELATIVE paths
# (e.g. "CMake Warning at CMakeLists.txt:131", "qtbase/cmake/...",
# "libs/cobalt/...Jamfile") that are indistinguishable by path from our own, so
# everything inside the window is dropped wholesale. Our own configure/build
# warnings fall before or after it. Windows builds open one window per config
# (Debug + RelWithDebInfo), so there can be several per log.
_CONAN_SECTION_START = "CMake-Conan: conan install"
_CONAN_SECTION_END = "======== Finalizing install (deploy, generators) ========"

# ctest writes the test-run output between these two markers (the second is its
# final summary line, printed whether tests pass or fail). That output is test
# execution, not a build, but a failing Boost.Test case prints
# "main.cpp(70): error: ... check ... has failed", which the compiler parsers
# would flag as a warning. The build warnings precede the run and the .deb
# packaging warnings (dpkg/dh/lintian) follow it, so dropping just this window
# removes the false hits without losing either.
_CTEST_SECTION_START = "Test project "
_CTEST_SECTION_END = "Total Test time (real) ="


def _relativise(path):
    """Strip the checkout/.deb-extraction prefix from an absolute path."""
    return _CHECKOUT_PREFIX_RE.sub("", path)


def _under_checkout(path):
    """True if an absolute path lies inside the checked-out source tree."""
    return "/safir-sdk-core/safir-sdk-core/" in path


def _process_cmake_line(normalised, cmake):
    """Return the kept/relativised CMake-warning line, or None to drop it."""
    cmake_path = cmake.group("path")
    # Relativise the "CMake Warning at <path>:<line>" location so the downstream
    # tool can link it to source after a fresh checkout.
    return _relativise(normalised) if cmake_path else normalised


def _process_line(line):
    """Return the (possibly rewritten) line to keep, or None to drop it.

    Backslashes are normalised to forward slashes so the same path logic works
    for the Windows logs.
    """
    normalised = line.replace("\\", "/")

    if _ALINK_RE.match(normalised):
        # Cosmetic .NET assembly-linker resource-path warning; never reported.
        return None

    cmake = _CMAKE_RE.match(normalised)
    if cmake is not None:
        return _process_cmake_line(normalised, cmake)

    match = _GCC_RE.match(normalised) or _MSVC_RE.match(normalised)
    if match is None:
        # Not a compiler diagnostic (lintian, dh_*, plain output, code context):
        # pass it through untouched so the lintian converter can still find it.
        return line

    path = match.group("path")
    if not _ABSOLUTE_PATH_RE.match(path):
        # Relative path => emitted relative to our build directory => our code.
        return normalised
    if _under_checkout(path):
        return _relativise(normalised)
    # Absolute path outside the checkout: a Conan dependency or a system header.
    return None


def _strip_sections(lines, start_marker, end_marker):
    """Drop every line from one containing start_marker through end_marker.

    Both marker lines are dropped. An unterminated window (the build died before
    end_marker) drops to end-of-log, which is fine for our markers: there is
    nothing of ours after a failed conan install or an aborted test run.
    """
    out = []
    in_section = False
    for line in lines:
        if not in_section and start_marker in line:
            in_section = True
            continue
        if in_section:
            if end_marker in line:
                in_section = False
            continue
        out.append(line)
    return out


def _is_noise_cmake_header(line):
    return _CMAKE_RE.match(line.replace("\\", "/")) is not None


def _strip_noise_cmake_blocks(lines):
    """Drop 'CMake Warning' headers whose body is known dependency/infra noise.

    analysis-model attaches the indented body to the header, so we look at the
    first non-empty body line and drop just the header when it matches a noise
    marker; the body lines that follow are plain text the parsers ignore.
    """
    out = []
    for index, line in enumerate(lines):
        if _is_noise_cmake_header(line):
            body = next((nxt.strip() for nxt in lines[index + 1:] if nxt.strip()), "")
            if any(marker in body for marker in _NOISE_CMAKE_BODY_MARKERS):
                continue
        out.append(line)
    return out


def _output_lines(text):
    """Yield the raw output lines of a build log, noise sections removed.

    Handles both the SDK build's buildlog.html (output wrapped in plain ``<pre>``
    blocks; the green command-echo blocks are skipped) and the plain stdout the
    examples/docs builds tee to a file (no HTML at all). The conan dependency
    builds and the ctest test run are dropped wholesale (see the marker
    constants), then known-noise CMake-warning blocks.
    """
    all_blocks = list(_PRE_BLOCK_RE.finditer(text))
    if all_blocks:
        lines = []
        for block in all_blocks:
            if block.group("attrs").strip():
                continue
            lines += html.unescape(block.group("body")).splitlines()
    else:
        lines = text.splitlines()
    lines = _strip_sections(lines, _CONAN_SECTION_START, _CONAN_SECTION_END)
    lines = _strip_sections(lines, _CTEST_SECTION_START, _CTEST_SECTION_END)
    return _strip_noise_cmake_blocks(lines)


def sanitize(text):
    """Convert build output into the filtered plain-text log."""
    out = [kept for kept in (_process_line(line) for line in _output_lines(text)) if kept is not None]
    return "\n".join(out) + ("\n" if out else "")


def _convert_file(in_path, out_path):
    try:
        with open(in_path, encoding="utf-8", errors="replace") as handle:
            text = handle.read()
    except OSError as exc:
        # A failed build row may not have produced a log; never crash on it.
        print(f"buildlog_to_text: skipping {in_path}: {exc}", file=sys.stderr)
        return
    with open(out_path, "w", encoding="utf-8") as handle:
        handle.write(sanitize(text))
    print(f"buildlog_to_text: wrote {out_path}")


def _convert_root(root):
    """Convert each root/<artifact>/buildlog.{html,txt} -> root/<artifact>.log.

    The SDK build uploads buildlog.html; the examples/docs builds tee plain
    stdout to buildlog.txt. Each download-artifact subdir holds exactly one.
    """
    for pattern in ("buildlog.html", "buildlog.txt"):
        for in_path in sorted(glob.glob(os.path.join(root, "*", pattern))):
            name = os.path.basename(os.path.dirname(in_path))
            _convert_file(in_path, os.path.join(root, name + ".log"))


def main(argv=None):
    """Convert a single buildlog, or every buildlog under --root, to a .log."""
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("input", nargs="?", help="a single buildlog.html to convert")
    parser.add_argument("output", nargs="?", help="output log path (with a single input)")
    parser.add_argument("--root", help="convert every <root>/*/buildlog.html to <root>/<dir>.log")
    args = parser.parse_args(argv)

    if args.root:
        _convert_root(args.root)
    elif args.input and args.output:
        _convert_file(args.input, args.output)
    else:
        parser.error("provide INPUT OUTPUT, or --root DIR")
    return 0


if __name__ == "__main__":
    sys.exit(main())
