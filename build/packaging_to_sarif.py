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
"""Convert the Debian .deb packaging warnings out of the build logs into SARIF.

The Quality Monitor / analysis-model toolchain that summarizes the compiler
warnings has no parser for the Debian packaging tools and no user-defined-regex
option, so we extract their findings ourselves and hand them on in the
tool-agnostic SARIF format it can ingest. Two kinds of line are collected:

* lintian, mirroring the regex the Jenkins warnings-ng setup used::

      W: safir-sdk-core-dev: no-manual-page [usr/bin/safir_build_common.py]
      E: safir-sdk-core: some-tag some explanatory text

* dpkg-* / dh_* (debhelper) warnings emitted during the package build::

      dpkg-shlibdeps: warning: diversions involved - output may be incorrect
      dh_cligacpolicy: warning: No Build-Depends(-Indep) on cli-common-dev!
      dh_auto_configure: warning: Use of debian/compat is deprecated ...

These run on every Linux build (ubuntu amd64/arm64 + debian), so the same
finding appears in several logs; results are de-duplicated.
"""
import argparse
import glob
import json
import os
import re
import sys

# lintian: mirrors the Jenkins parser regex (^(E|W): pkg: tag msg), with the
# package/tag character classes widened to match real Debian names (digits, '.',
# '+'). The remainder is the message, which may end with a "[path]" locator.
_LINTIAN_RE = re.compile(r"^(?P<severity>[EW]): (?P<pkg>[a-z0-9.+-]+): (?P<tag>[a-z0-9.+-]+)(?: (?P<msg>.*))?$")

# dpkg-* / dh_* tooling warnings: "<tool>: warning: <message>". The leading tool
# name (e.g. dpkg-shlibdeps, dh_cligacpolicy, dh_auto_build) becomes the rule.
_DEBHELPER_RE = re.compile(r"^(?P<tool>(?:dpkg-[a-z]+|dh_[a-z_]+)): warning: (?P<msg>.*)$")

# Trailing "[some/path]" in a lintian message names the offending file.
_LOCATION_RE = re.compile(r"\[([^\]]+)\]\s*$")

# Known-benign packaging lines to drop. We only suppress findings that are
# provably outside our control and provably harmless, so the report can stay
# completely clean:
#
# * dpkg-shlibdeps "diversions involved" is emitted because libc6 diverts the
#   dynamic loader /lib64/ld-linux-x86-64.so.2 -> *.usr-is-merged as part of the
#   Debian /usr-merge (DEP-17) transition. dpkg-shlibdeps is conservative and
#   warns whenever any diversion is in play, but the libc6 dependency it computes
#   is correct. It originates in the build distro's libc packaging, not in our
#   debian/* (dpkg bug #1035904), so there is nothing we can fix here. The
#   follow-up " diversion by libc6 ..." detail lines have no "tool: warning:"
#   prefix and are already ignored.
_IGNORED_RES = (
    re.compile(r"^dpkg-shlibdeps: warning: diversions involved - output may be incorrect$"),
)

_SEVERITY = {"E": "error", "W": "warning"}


def _parse_line(line):
    """Return a finding dict for a packaging warning line, or None."""
    stripped = line.rstrip()

    if any(ignore.match(stripped) for ignore in _IGNORED_RES):
        return None

    match = _LINTIAN_RE.match(stripped)
    if match is not None:
        msg = match.group("msg") or ""
        location = _LOCATION_RE.search(msg)
        # Prefer the in-package path as the location; fall back to the package
        # name (what the Jenkins parser used) when lintian gives no path.
        uri = location.group(1) if location else match.group("pkg")
        return {
            "tool": "lintian",
            "rule": match.group("tag"),
            "severity": _SEVERITY[match.group("severity")],
            "uri": uri,
            "text": f"{match.group('pkg')}: {match.group('tag')} {msg}".rstrip(),
        }

    match = _DEBHELPER_RE.match(stripped)
    if match is not None:
        tool = match.group("tool")
        return {
            "tool": tool,
            "rule": tool,
            "severity": "warning",
            "uri": tool,
            "text": f"{tool}: {match.group('msg')}".rstrip(),
        }

    return None


def parse_logs(texts):
    """Parse many log texts into a de-duplicated, ordered list of findings."""
    findings = []
    seen = set()
    for text in texts:
        for line in text.splitlines():
            finding = _parse_line(line)
            if finding is None:
                continue
            key = (finding["tool"], finding["rule"], finding["uri"], finding["text"])
            if key in seen:
                continue
            seen.add(key)
            findings.append(finding)
    return findings


def to_sarif(findings):
    """Build a minimal SARIF 2.1.0 document from the findings."""
    rules = {}
    results = []
    for finding in findings:
        rules.setdefault(finding["rule"], {"id": finding["rule"]})
        results.append({
            "ruleId": finding["rule"],
            "level": finding["severity"],
            "message": {
                "text": finding["text"]
            },
            "locations": [{
                "physicalLocation": {
                    "artifactLocation": {
                        "uri": finding["uri"]
                    }
                }
            }],
        })
    return {
        "$schema": "https://json.schemastore.org/sarif-2.1.0.json",
        "version": "2.1.0",
        "runs": [{
            "tool": {
                "driver": {
                    "name": "Debian packaging",
                    "rules": list(rules.values())
                }
            },
            "results": results,
        }],
    }


def main(argv=None):
    """Parse the given (or --root) logs and write a SARIF file of the findings."""
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("logs", nargs="*", help="sanitized log files to scan")
    parser.add_argument("--root", help="scan every <root>/*.log")
    parser.add_argument("-o", "--output", default="packaging.sarif", help="SARIF output path")
    args = parser.parse_args(argv)

    paths = list(args.logs)
    if args.root:
        paths += sorted(glob.glob(os.path.join(args.root, "*.log")))

    texts = []
    for path in paths:
        try:
            with open(path, encoding="utf-8", errors="replace") as handle:
                texts.append(handle.read())
        except OSError as exc:
            print(f"packaging_to_sarif: skipping {path}: {exc}", file=sys.stderr)

    findings = parse_logs(texts)
    # Always emit a (possibly empty) SARIF so the warnings-summary job stays green
    # even when there are no logs at all - e.g. a cancelled run or one where every
    # build failed, so download-artifact created nothing and the output directory
    # does not exist yet.
    out_dir = os.path.dirname(args.output)
    if out_dir:
        os.makedirs(out_dir, exist_ok=True)
    with open(args.output, "w", encoding="utf-8") as handle:
        json.dump(to_sarif(findings), handle, indent=2)
    print(f"packaging_to_sarif: wrote {len(findings)} finding(s) to {args.output}")
    return 0


if __name__ == "__main__":
    sys.exit(main())
