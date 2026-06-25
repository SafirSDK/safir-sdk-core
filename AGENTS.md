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
```bash
# Run all tests via CTest (after building)
ctest

# Skip slow tests (recommended for quick iterations)
SAFIR_SKIP_SLOW_TESTS=1 ctest
```

Slow tests skipped by `SAFIR_SKIP_SLOW_TESTS`: LowLevelLogger, Communication tests, ElectionHandler tests, DOPE backend tests, restart/light node tests, WebSocket tests, system picture tests, Incarnation_And_Control_Tests, sate_script.

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
release with the installers on version-tag pushes. The whole workflow runs with
`SAFIR_SKIP_SLOW_TESTS=1`.

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

Not yet migrated / still missing on GitHub Actions:
- **Documentation rendering.** The doc-generation step (doxygen/dia/texlive/
  asciidoctorj) is not run in the GHA workflow yet.
- **The full ("slow") unit tests.** GHA runs with `SAFIR_SKIP_SLOW_TESTS=1`, so
  the slow tests listed under *Running Tests* above never run there. The
  intended fix is to **break the slow tests out into a separate test suite that
  is launched on its own, the way the dose test suites are** — rather than
  leaving multi-hour cases inside the unit-test run (a unit test that takes
  hours is not really a unit test). This is a sizeable undertaking and has not
  been started.
- **Build-warnings analysis and the quality gate.** Jenkins'
  `archive_and_analyze()` scans `buildlog.html` for GCC, MSBuild, CMake, Java,
  Doxygen and **lintian** (`.deb` packaging) warnings via `scanForIssues`/
  `publishIssues`, and applies a quality gate (`threshold: 1, TOTAL → unstable`)
  so a single new warning marks the build unstable. GHA only uploads
  `buildlog.html` as an artifact; it never parses it, so there is no warnings
  gate and no lintian check. This is the main quality signal Jenkins enforces
  that GHA does not.

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
