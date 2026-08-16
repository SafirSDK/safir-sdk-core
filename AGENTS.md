# AGENTS.md

This file provides guidance to AI agents when working with code in this repository.

## Project Overview

Safir SDK Core is a middleware and platform for creating distributed soft real-time systems. It provides scalable, reliable, and portable data distribution for real-time and information systems, developed over 25+ years at Saab. The SDK supports multi-language development (C++, C#, Java).

Key characteristics:
- No infinite queues, event-driven (no polling), asynchronous (no RPC/blocking calls)
- Designed for bounded latency
- Implements a distributed object cache with shared memory for local access
- UDP/IP with reliable protocol for inter-node communication

## Build Commands

### Linux (Debian/Ubuntu)
```bash
# Install prerequisites (Ubuntu 24.04/Debian 12+)
sudo apt-get install python3 pipx python-is-python3 python3-distro build-essential \
     g++ cmake default-jdk asciidoctor cli-common-dev libboost-all-dev unixodbc-dev doxygen \
     graphviz qt6-base-dev qt6-websockets-dev qt6-base-private-dev qt6-svg-dev dia dblatex \
     devscripts debhelper fakeroot ninja-build python3-websocket texlive-font-utils \
     ghostscript

# Install Conan 2
pipx install "conan>=2.5.0"

# Build AND package (creates .deb files in tmp/). build.py only builds-and-
# packages; there is no build-without-package mode. Shared logic lives in
# safir_build_common.py.
build/build.py

# Jenkins-style build (more verbose, obeys the build-matrix variables)
build/build.py --jenkins

# Manual CMake build (for other Linux distributions)
cmake . -DCMAKE_BUILD_TYPE=Release
make
make install
```

To build an external user dou-project, use `dobmake_batch.py` (installed as
`dobmake-batch`). To just build the source tree as a developer, use cmake/ninja
directly (see BUILD.Linux.txt / BUILD.Windows.txt).

### Running Tests

There are two categories of tests, run two different ways.

**Fast tests — CTest.**
```bash
# Run all tests via CTest (after building)
ctest
```
Every ctest test now runs by default; the truly slow, multi-process cases were
moved into the installed slow suite (below), so there is no longer a skip switch.

**Slow system tests — the installed TestSuite.** The big "population 1"
system-level tests (system picture, incarnation/control, light/restart nodes,
lowmem, DOPE none/file backends, tracer backdoor, election handler) have been
moved out of ctest into the TestSuite install component, so they run against an
*installed* package the way the dose suite does — a hours-long case is not
really a unit test. Run the whole set with a single command after installing:
```bash
make install            # or: ninja install
run_slow_tests          # runs the whole slow suite; see run_slow_tests --list / --help
```
`run_tracer_backdoor_tests` needs the `websockets` pip package; the odbc DOPE
backend needs a database and is opt-in (`run_slow_tests --include-odbc`, or in
CI `run_test.py --test database`). Some tests (system picture, light nodes) need
working multicast loopback on the host; `run_slow_tests` checks this up front and
refuses to run them if it is missing (override with `--ignore-multicast-check`;
a typical dev-host fix is `sudo ip route add 239.0.0.0/8 dev lo`). The dose tests
are a separate installed suite (`run_dose_tests`).

### CI/CD

Two CI systems run against the repository.

**GitHub Actions** (`.github/workflows/ci.yml`) runs on pushes to
master/develop/feature/private branches and on pull requests. A matrix builds
and packages across ubuntu-noble (amd64 + arm64), debian-trixie, vs2022 and
vs2026. There is no Debian-labelled runner, so the debian-trixie row builds
inside a `debian:13` container on the ubuntu-latest host (with `--shm-size`,
because dose_main needs a 100 MB `/dev/shm`). Each row runs
`build/build.py --jenkins`, then downstream jobs install the produced package
and run the example builds and the dose test suites (standalone, multinode, and
multicomputer). The multicomputer run joins two hosted runners over an
accountless WireGuard overlay (`.github/actions/wireguard-overlay`), so the node
under test on a native runner talks to three debian slave containers on a
second runner. Every job uploads its JUnit results as artifacts; a final
`test-summary` job aggregates them into one GitHub Check (via
`EnricoMi/publish-unit-test-result-action`). A `release` job drafts a GitHub
release with the installers on version-tag pushes. A `workflow-lint` job runs
zizmor over the workflow/action files (see the migration notes below).

> **The platform matrix is duplicated across jobs — keep them in sync.** GitHub
> Actions does not support YAML anchors, so each job spells out its own
> `strategy.matrix`. The same platform table appears (in three slightly
> different shapes) in `build`, `build-examples`, `dose-tests`, `slow-tests`,
> `multicomputer-master` and `multicomputer-slaves`. `build`/`build-examples`
> are identical to each other (they carry `conan_home` and key the platform
> without the arch suffix); `dose-tests`/`slow-tests` are identical to each
> other (combined `ubuntu-noble-amd64` platform token + separate
> `platform_name`); the multicomputer jobs use a 4-platform subset (no debian).
> When you add or rename a platform, change a runner label, or bump a container
> image / `--shm-size`, **update every job's matrix**, not just the one you are
> looking at. We deliberately keep them inline (a `fromJSON`-from-setup-job
> generator was considered and rejected: the churn is low and the indirection
> hurts readability more than the duplication does).

**Jenkins** (`Jenkinsfile`) is the canonical release build. Matrix build across:
- **Platforms**: ubuntu-noble, debian-trixie, vs2022, vs2026
- **Architectures**: amd64 (x86 dropped for most platforms)
- **Package types** (`PACKAGE_TYPE` axis): Full (ships both Debug and RelWithDebInfo MSVC-runtime flavours), DebugOnly

Test stages:
1. Build and Unit Test
2. Standalone Tests
3. Multinode Tests
4. Multicomputer Tests (cpp only, requires debian-trixie)
5. Build Examples

### Jenkins → GitHub Actions migration status

The goal is for GitHub Actions to fully replace Jenkins as the canonical CI.
What has moved over so far:
- Build and package across all platforms (incl. native arm64, which Jenkins
  does not do).
- Unit tests (run by `build.py --jenkins` via ctest), **minus the slow tests** —
  see below.
- The standalone, multinode and multicomputer dose test suites.
- Build Examples.
- Drafting a GitHub release with installers on version-tag pushes.
- Documentation rendering (User's Guide + Requirements Specification, HTML +
  PDF), via a standalone `render-docs` job on ubuntu-latest. It runs the
  asciidoctor/dia/dblatex toolchain directly (no container) and uploads the
  rendered docs as an artifact.
- Workflow security hardening with [zizmor](https://github.com/woodruffw/zizmor),
  wired in as the `workflow-lint` job (runs `zizmor .github/`, version pinned).
  **Whenever you edit anything under `.github/` (workflows or composite
  actions), re-run `zizmor .github/` and keep it clean before committing** — the
  CI job will fail otherwise. Deliberate exceptions are recorded as inline
  `# zizmor: ignore[<rule>]` comments with a rationale, plus the `unpinned-uses`
  policy in `.github/zizmor.yml` (third-party actions must be hash-pinned;
  first-party `actions/*` may float on major tags).
- **Build-warnings analysis**, via a `warnings-summary` job that replaces
  Jenkins' `archive_and_analyze()`. Each build/examples/docs job uploads its log
  as a `buildlog-*` artifact; `build/buildlog_to_text.py` unwraps it and **drops
  warnings from Conan dependency builds** — both by path (dependency/system
  paths outside the checkout) and by stripping the whole `conan install` …
  `Finalizing install` section (where dependency CMake/Boost/meson warnings have
  relative paths indistinguishable from ours) — leaving only our own tree's
  diagnostics, paths made repo-relative for source links.
  `build/packaging_to_sarif.py` extracts the `.deb` packaging warnings (lintian
  plus the `dpkg-*`/`dh_*` tooling warnings) to SARIF, since the analysis tool
  has no parser for them. [Quality
  Monitor](https://github.com/uhafner/quality-monitor) — same `analysis-model`
  parsers as the Jenkins warnings-ng plugin — then publishes one "Build warnings"
  Check with source-linked annotations covering GCC, MSBuild, CMake, Java,
  Doxygen and the packaging warnings. lintian was silently absent from the Linux
  builds (it is only a `devscripts` *recommends*, dropped by
  `--no-install-recommends`); it is now installed in `setup-build-env` so
  `debuild` runs it as it does on Jenkins. (ALINK `A99999` .dll.policy
  resource-path warnings from the .NET assembly linker are deliberately not
  collected — they are cosmetic.)

Not yet migrated / still missing on GitHub Actions:
- **The full ("slow") unit tests — done.** The big, multi-process system-level
  cases were broken out into the installed TestSuite component and run via
  `run_slow_tests` (see *Running Tests*), launched in CI by the `slow-tests` job
  (`run_test.py --test slow-tests`) across the full platform matrix (ubuntu-noble
  amd64/arm64, debian-trixie, vs2022/vs2026). Each driver has its own wall-clock
  timeout so a hung test is killed and reported instead of stalling the whole
  suite. Every remaining ctest test is fast enough to run inline, so the old
  `SAFIR_SKIP_SLOW_TESTS` skip switch was removed entirely.
- **The build-warnings quality gate.** Jenkins applied a quality gate
  (`threshold: 1, TOTAL → unstable`) so a single new warning marked the build
  unstable. The GHA `warnings-summary` job (above) reports the warnings but sets
  no quality gate, because GitHub has no "unstable" build state — the Check goes
  green and the warnings are informational. Enforcing a gate would mean either
  failing the job outright (stricter than Jenkins ever was) or publishing a
  neutral Check via the Checks API; a gate can be turned on later by adding a
  `quality-gates` block to the job's Quality Monitor config.

Deliberate-or-pending coverage differences (decide before retiring Jenkins):
- **`PACKAGE_TYPE = DebugOnly`.** Jenkins runs a `Full × DebugOnly` axis across
  Build, Test suite and Build examples; GHA only ever builds and tests `Full`
  (the default). The DebugOnly path — chiefly the Windows debug-runtime-only
  packaging — is not exercised on GHA.
- **32-bit (x86) builds.** Jenkins still keeps `debian-trixie` × `x86` in its
  matrix (x86 is dropped only for ubuntu/vs2022/vs2026), gated on an x86 agent
  being online. GHA has no x86 at all. This may be an intentional drop, but it
  should be a conscious decision rather than a silent omission.
- **Generic unit-test output.** Jenkins zips `**/test_output/**` from the
  unit-test stage; GHA archives only JUnit XML plus the dose `dose_test_output`,
  so non-dose ctest output is not captured.
- **Conan cache-save warnings.** Every build/examples job ends with a
  `##[warning]Cache save failed … Unable to reserve cache with key … another
  job may be creating this cache`. This is benign and pre-dates the warnings
  work: the Conan cache key (`conan-v2-<platform>-<arch>-<hash>`) only changes
  when dependencies change, and GitHub cache keys are write-once — so the first
  run on a branch saves the cache and every later run with unchanged deps is
  rejected because the key already exists (it is *not* a race between matrix
  rows; each platform has its own key). Harmless, but the yellow annotation is
  noisy. It cannot be filtered by the warnings pipeline (it is a runner
  annotation, never written to `buildlog.html`); silencing it would mean making
  the cache step save only on a miss (e.g. skip save when the restore was an
  exact hit). Left as a follow-up.

Opportunities the GHA setup newly makes practical:
- **DOPE ODBC tests.** These have not run in Jenkins for a long time, because
  maintaining a database setup for them was abandoned. GitHub-hosted runners
  ship with databases preinstalled, so reintroducing an ODBC test job is much
  more tractable now than it was under Jenkins.

### Shared Library ABI Classification

Every non-imported `SHARED` library defined in the tree must be classified by
ABI flavor in its `CMakeLists.txt`, or CMake configuration fails with a
`FATAL_ERROR` (see `src/cmake/SafirLibraryAbi.cmake`):
- `safir_mark_dual_abi(<target>)` — libraries with a C++ public ABI. On MSVC
  these ship in both Debug and RelWithDebInfo MSVC-runtime flavours; the `d`
  debug postfix is applied automatically.
- `safir_mark_single_abi(<target>)` — C-ABI / JNI / runtime-only libraries.
  Clears the MSVC debug postfix so the filename stays stable for name-based
  loaders (JNI, `dlopen`).

A newly added SHARED library must call exactly one of these.

## Architecture

### The Dob (Distributed Objects)

The Dob provides three distribution mechanisms:

1. **Messages** - Fire-and-forget, no guaranteed delivery, no storage
2. **Services** - Request/response with guaranteed delivery and timeout handling
3. **Entities** - Owned objects stored in Dob, with subscriptions and guaranteed delivery

### Layered Component Structure

Components are suffixed `.ss` (Safir Subsystem):

1. **LLUF (Low Level Utilities Framework)** - Foundation layer
   - `lluf_config.ss` - Configuration management
   - `lluf_utils.ss` - Utility functions
   - `lluf_crash_reporter.ss` - Crash reporting

2. **DOTS (Distributed Object Type System)** - Type system layer
   - `dots_kernel.ss` - Core type system kernel
   - `dots_cpp.ss`, `dots_dotnet.ss`, `dots_java.ss` - Language bindings
   - `dots_dobmake.ss` - Code generation tool for DOU files

3. **DOSE (Distributed Object Service Engine)** - Main middleware
   - `dose_main.ss` - Core DOSE service (started by safir_control)
   - `dose_cpp.ss`, `dose_dotnet.ss`, `dose_java.ss` - Language bindings

4. **Supporting Components**
   - `dope/` - Object persistence engine (dope_main)
   - `swre/` - Software reports/telemetry
   - `web/` - REST and WebSocket interface (safir_web)

### Key Directories
- `include/Safir/` - Public C++ headers (Dob, Logging, Application, Utilities)
- `src/` - Source code organized by component
- `src/tests/` - Integration tests
- `examples/` - Example applications (vehicleapp, vehiclemmi)
- `docs/users_guide/` - User's Guide (AsciiDoc format)

## DOU Files (Type System)

Data Object Units (DOU/DOM files) define the type system in XML:
- Inherit from `Safir.Dob.Entity`, `Safir.Dob.Message`, `Safir.Dob.Service`, etc.
- Members have IsNull and IsChanged flags
- Support for Items (complex types), Structs, Arrays, Sequences, Dictionaries
- Parameters are runtime constants read at startup

The `dobmake` tool generates language-specific code from DOU files.

## Configuration

### INI Files
Three main configuration files searched in order:
1. `/etc/safir-sdk-core/` (Linux system-wide)
2. `~/.config/safir-sdk-core/` (Linux user)

- **locations.ini** - Lock files, crash dumps, IPC endpoints
- **logging.ini** - Syslog settings, native logging
- **typesystem.ini** - Shared memory size, DOU search paths, library modules

### SAFIR_INSTANCE
Environment variable to run multiple Dob nodes on one computer. All processes with the same SAFIR_INSTANCE value form one node.

## Running the System

```bash
# Start the Dob (required before any application can connect)
safir_control

# Check system status
safir_status

# GUI for node control
safir_control_gui

# CLI for stop commands
safir_control_cli --help
```

## Debugging Tools

- **Sate** (Safir Application Tester) - GUI for interactive Dob operations, entity subscriptions, message sending. Supports scripting via JSON files.
- **Dobexplorer** - Shows memory usage graphs, node statuses, connection statistics
- **safir_tool_launcher** - Launches debug tools with specific SAFIR_INSTANCE
- **dots_configuration_check** - Validates DOU files, queries typesystem
- **Tracer** - Debug logging with `Safir.Application.Tracer` class
  - Control via `bd` command, Safir Tracer Viewer GUI, or `FORCE_LOG` env var
  - Outputs: stdout, Safir Logging, Tracer UDP protocol

## Code Style

### Python
- Formatter: YAPF with PEP8 base style, 120 character line limit
- Linter: PyLint (score threshold: 10.0)
- Naming: snake_case for functions/variables, PascalCase for classes

### C++
- Uses CMake build system
- Optional Clang-Tidy static analysis (RUN_CLANG_TIDY flag)
- Public headers in `include/Safir/`

## Multi-Language Support

The SDK provides bindings for:
- **C++** (native/primary)
- **C#/.NET** (modules suffixed `_dotnet`)
- **Java** (modules suffixed `_java`)

### Interface Design Philosophy

**IMPORTANT**: Public interfaces are available in C++, C#, and Java. When working on these interfaces, you must ensure they remain as similar as possible across all three languages, while preserving the conventions and idioms of each language. Interfaces should feel native to each language without introducing surprises.

When modifying or creating public interfaces, ensure:
- Following language-specific naming conventions (e.g., PascalCase methods in C#, camelCase in Java, snake_case or PascalCase in C++ as appropriate)
- Using language-native patterns (e.g., properties in C#, getters/setters in Java)
- Maintaining consistent behavior and semantics across all language bindings

Note for Java: Use `putVal()`/`putObj()` instead of `put()` when adding to collections.
