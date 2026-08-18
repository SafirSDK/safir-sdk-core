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
Every ctest test now runs by default; there is no longer a skip switch. The
hours-long, multi-process "population 1" cases were moved into the installed slow
suite (below). A handful of shorter multi-process tests still run inline in ctest
(WebSocket component/stress, performance_test, sate_script, RawHandler_test,
StopHandler_test) — they were in the old skip list but are cheap enough (~15 s
each) to keep here.

**Slow system tests — the installed TestSuite.** The big "population 1"
system-level tests (system picture, incarnation/control, light/restart nodes,
lowmem, DOPE none/file backends, tracer backdoor, election handler,
communication) have been
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

Each driver writes a JUnit report (`<driver>.junit.xml`, one `<testcase>` per
named case) via the shared `JUnitReporter` in
`src/tests/test_support/python/junit.py`; `run_slow_tests` gives every driver its
own report directory (`SAFIR_SLOW_TEST_JUNIT_DIR`) and, like `run_dose_tests`,
distinguishes a *test-case* failure (exit 1 — carried by the junit, CI job stays
green, the "Test results" check goes red) from an *infra* failure (a driver that
couldn't run, crashed, or hung → exit 2, fails the job). In CI the `slow-tests`
job uploads the reports so they feed the same consolidated `test-summary` Check
as the ctest and dose suites.

### Windows Defender false positives

Defender has flagged a freshly built `safir_control.exe` as
`Exploit:Win64/Facupel!dha`, failing `TryStart_safir` with `OSError [WinError
225]` (`ERROR_VIRUS_INFECTED`, raised by `CreateProcess` — the binary never
runs, so this is not a code fault). Seen 2026-08-17 on a vs2022 development
build.

Confirmed a reputation/behaviour false positive, not a compromised Conan
package: `conan cache check-integrity "*"` passed, a Defender scan of the cache
and build tree was clean, and no other binary was flagged (16 targets link the
same breakpad/boost code). Our Windows binaries trip heuristics because they
install a breakpad exception handler (`lluf_crash_reporter`) and spawn hidden
child processes (`ControlApp.cpp`) while unsigned and with no hash reputation.

**Decision: no action taken.** Detection keys on the file hash, so every build
is a new unknown binary and this could recur on any Windows build, but it is
rare enough not to chase. Whether the GHA vs2022/vs2026 rows are exposed is
unverified: they have never hit it, and we have not checked whether Defender's
real-time protection is active on those images (`Get-MpComputerStatus` on a
Windows row would settle it). Note that nothing in CI disables Defender
antivirus — the `netsh advfirewall` call in the multicomputer jobs turns off the
Defender *firewall*, a different component. If it recurs: verify the cache as
above, then `Add-MpPreference -ExclusionPath <workspace>` on that machine.
Authenticode-signing released binaries is the real fix if end users start
hitting it.

### CI/CD

Two CI systems run against the repository: **GitHub Actions** (the target CI)
and **Jenkins** (`Jenkinsfile`, still the canonical release build). The goal is
for GitHub Actions to fully replace Jenkins.

**GitHub Actions** (`.github/workflows/ci.yml`) runs on pushes to
master/develop/feature/private branches and on pull requests. A matrix builds
and packages across ubuntu-noble (amd64 + arm64), debian-trixie, vs2022 and
vs2026; there is no Debian-labelled runner, so debian-trixie builds inside a
`debian:13` container on ubuntu-latest (with `--shm-size`, because dose_main
needs a 100 MB `/dev/shm`). Each row runs `build/build.py --jenkins`; downstream
jobs install the package and run the example builds, the dose test suites
(standalone, multinode, and multicomputer — the last joins two runners over an
accountless WireGuard overlay so a native node talks to three debian slave
containers), and the installed slow-test suite. A `test-summary` job aggregates
JUnit results into one Check, a `release` job drafts a release on version-tag
pushes, `render-docs` renders the guides, and `workflow-lint` runs zizmor.

**Rules when editing CI** (ignore either and the build breaks or rots silently):
- **`.github/` changes → run `zizmor .github/` and keep it clean before
  committing** (the `workflow-lint` job fails otherwise). Deliberate exceptions
  are inline `# zizmor: ignore[<rule>]` comments with a rationale; third-party
  actions must be hash-pinned (`unpinned-uses` policy in `.github/zizmor.yml`),
  first-party `actions/*` may float on major tags.
- **The platform matrix is duplicated — keep every copy in sync.** GitHub
  Actions has no YAML anchors, so `build`, `build-examples`, `dose-tests`,
  `slow-tests`, `multicomputer-master` and `multicomputer-slaves` each spell out
  their own `strategy.matrix` (`build`≡`build-examples`, keyed on platform
  without the arch suffix and carrying `conan_home`; `dose-tests`≡`slow-tests`,
  using a combined `ubuntu-noble-amd64` token + separate `platform_name`; the
  multicomputer jobs are a debian-less 4-platform subset). When you add/rename a
  platform, change a runner label, or bump a container image / `--shm-size`,
  update **every** job. (A `fromJSON`-from-setup-job generator was considered and
  rejected — churn is low and the indirection hurts readability more.)

**Jenkins** (`Jenkinsfile`) matrix: platforms ubuntu-noble / debian-trixie /
vs2022 / vs2026; amd64 (plus x86 on debian-trixie); `PACKAGE_TYPE` axis Full
(both MSVC-runtime flavours) and DebugOnly. Stages: Build + Unit Test,
Standalone, Multinode, Multicomputer (cpp only, debian-trixie), Build Examples.

#### Migration status (reference — kept until Jenkins is retired)

**Moved over:** build+package on all platforms (incl. native arm64, which
Jenkins can't do); ctest unit tests (via `build.py --jenkins`); the slow tests
(now the `slow-tests` job across the full matrix, each driver wall-clock-timed,
so `SAFIR_SKIP_SLOW_TESTS` is gone); standalone/multinode/multicomputer dose
suites; Build Examples; release drafting; doc rendering (`render-docs`, runs the
asciidoctor/dia/dblatex toolchain directly); zizmor hardening (`workflow-lint`);
build-warnings analysis (`warnings-summary`: `buildlog_to_text.py` drops
Conan-dependency warnings by path and by stripping the `conan install` …
`Finalizing install` block, `packaging_to_sarif.py` extracts lintian/dpkg/dh
warnings to SARIF, and Quality Monitor publishes one source-linked Check;
lintian is now installed in `setup-build-env` so `debuild` runs it as on
Jenkins; cosmetic ALINK `A99999` .dll.policy warnings are dropped).

**Pending / decide before retiring Jenkins:**
- **No build-warnings quality gate.** `warnings-summary` reports but does not
  gate (GitHub has no "unstable" state); enforce later via a `quality-gates`
  block on the Quality Monitor job.
- **`PACKAGE_TYPE = DebugOnly` not built on GHA** (only Full) — the Windows
  debug-runtime-only packaging path is unexercised.
- **No 32-bit (x86) on GHA**; Jenkins still keeps debian-trixie × x86. Confirm
  this is an intentional drop, not a silent one.
- **Generic ctest output not archived** — GHA keeps only JUnit XML + the dose
  `dose_test_output`, not `**/test_output/**`.
- **Benign Conan "Cache save failed … another job may be creating this cache"
  annotation** on every build (write-once cache key already saved; harmless but
  noisy, can't be filtered as it's a runner annotation). Fix = save-on-miss-only.

**Newly practical:** a DOPE ODBC test job — hosted runners ship databases
preinstalled, so it's far more tractable than under Jenkins (abandoned there).

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
