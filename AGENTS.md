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

Known intermittent test failures — which tests flake, why, and how to tell a
flake from a regression — are catalogued in [TEST_STATUS.md](TEST_STATUS.md). A
single red CI run is usually a known flake; check there before treating it as a
regression.

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

Since 2026-08-17 Defender flags **Debug-built** `safir_control.exe` as
`Exploit:Win64/Facupel!dha`. `CreateProcess` then refuses to start the image, so
`TryStart_safir` fails with `OSError [WinError 225]` (`ERROR_VIRUS_INFECTED`).
The binary never runs, so this is not a code fault.

**Scope: the `PACKAGE_TYPE = DebugOnly` Jenkins rows only** — the sole
configuration that builds a Debug `safir_control.exe` and runs ctest against it.
In a `Full` Windows build the Debug pass builds only the `safir_dual_abi_libs`
target and skips tests altogether (`safir_build_common.py:562-565`, `:608-617`),
so the package ships RelWithDebInfo executables plus debug dual-ABI *libraries*,
never debug executables. The published 7.4.2 installer was checked and is clean,
so **users are unaffected**. GHA is unaffected too, but only because it builds
`Full` alone — see the DebugOnly item under Migration status.

**Confirmed a false positive, not a compromised dependency.** Decisive test:
`safir_control`'s sources are byte-identical between 7.4.2 and HEAD, both pin
`boost/1.86.0`, and 7.4.2 built cleanly when it was released — so nothing changed
on our side and Defender's rule set did. It reproduces on two separate build
machines, on a private branch, on develop and on master. VirusTotal returns a
single detection across the whole engine set, Microsoft only. `conan cache
check-integrity "*"` passed and a Defender scan of the cache and build tree was
clean. The exe's entire content is its own three translation units plus static
`Boost::filesystem`/`Boost::program_options` (every Safir library it links is
SHARED), and ~30 other targets link that same static boost without being
flagged. The rule is presumably reacting to an unsigned binary that spawns a
hidden child process (`ControlApp.cpp`) and installs a console control handler
(`TerminateHandler.cpp`).

**Decision: reported to Microsoft as an incorrect detection on 2026-08-20**
(<https://www.microsoft.com/en-us/wdsi/filesubmission>); otherwise treated as low
priority, since it costs one Jenkins axis on a system being retired, with no user
or GHA impact. If Microsoft revises the rule, the detection simply stops firing
after a definitions update — re-run a `DebugOnly` Windows build to confirm. No
workaround was applied; `Add-MpPreference -ExclusionPath <workspace>` on the
affected agent is available if the red builds get in the way. To re-settle this
if it recurs, rebuild an older tag whose release build was clean: the same
sources flagged today means Defender changed, not us. Note that nothing in CI
disables Defender antivirus — the `netsh advfirewall` call in the multicomputer
jobs turns off the Defender *firewall*, a different component.

### CI/CD

Two CI systems run against the repository: **GitHub Actions** (the target CI)
and **Jenkins** (`Jenkinsfile`, still the canonical release build). The goal is
for GitHub Actions to fully replace Jenkins.

**GitHub Actions** (`.github/workflows/ci.yml`) runs on pushes to
master/develop/feature/private branches and on pull requests. A matrix builds
and packages across ubuntu-noble (amd64 + arm64), debian-trixie, vs2022 and
vs2026; there is no Debian-labelled runner, so debian-trixie builds inside a
`debian:13` container on ubuntu-latest (with `--shm-size`, because dose_main
needs a 100 MB `/dev/shm`).

Every runner here is GitHub-hosted; this project has no self-hosted runners. In
particular **`windows-2025-vs2026` is a normal GitHub-hosted image**, despite the
unusual-looking label — do not read it as self-hosted, and do not "simplify" it
to a plain `windows-2025`: the suffixed label is what selects the image carrying
the VS2026 toolchain.

Each row runs `build/build.py --jenkins`; downstream
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
- **Every package install goes through the `retry` wrapper.** Each one fetches
  from a third party we do not control (the Ubuntu archive, chocolatey,
  sourceforge, maven, PyPI), and a failure there kills a whole matrix leg in its
  setup step before anything is compiled or tested, skipping every dependent job.
  `.github/actions/retry.sh` defines `retry`; source it as
  `"${GITHUB_ACTION_PATH}/../retry.sh"` and pass a *function*, and do not add a
  bare `apt-get install`, `choco install` or `pip install`. It lives one level
  above the actions because `GITHUB_ACTION_PATH` resolves to the directory of the
  action doing the sourcing, so a shared helper cannot sit inside any one of
  them. One policy for all of them — **2 hours of wall clock, or 13 attempts,
  whichever runs out first**, spaced 30s doubling to a 15-minute cap —
  deliberately uniform so there is a single number to reason about. The budget is
  wall clock rather than the sum of the sleeps because a hung install costs
  whatever its own timeout is, which dwarfs the sleeps; bounding the sleeps let
  run 33111295598 spend six hours in one setup step.
- **Retry each package separately, and keep the retried unit as small as the
  thing that fails.** Anything conditioned on `RETRY_ATTEMPT` — notably the `-f`
  that `setup-build-env` adds to `choco` from the second attempt on — applies to
  the whole unit, so batching independent packages means one package's transient
  failure changes how all the others are installed next time round. That is what
  broke run 33111295598: a sourceforge 404 on `doxygen.install` made attempt 2
  re-run the batch with `-f`, forcing a reinstall of `dejavufonts`, whose install
  script does a flagless `Shell.Application` `CopyHere` into the Fonts folder —
  which, with the fonts already installed by attempt 1, waits forever on an
  overwrite dialog that no runner can answer. Also pass `choco` an
  `--execution-timeout` well under its 2700s default so a hang costs minutes.
  The Linux exception: retry `update`+`install` as one unit, because a
  stale-index 404 is not fixed by re-running `install`. See TEST_STATUS.md →
  "Third-party package fetches" for the failures that prompted all this.

  Covered so far: `setup-build-env` (apt, choco, pip) and `setup-test-env` (apt,
  pip). **Still bare**, and worth wrapping if they ever bite: the four Debian
  container bootstraps and the multicomputer slave container in `ci.yml` (they
  run *before* checkout, or inside a `docker run` string, so they cannot source
  the helper as-is), the docs-toolchain install in the `render-docs` job, and
  `wireguard-overlay`'s apt/choco — that last one needs thought rather than a
  copy-paste, since a long retry on one side of the overlay eats into the peer's
  `peer-wait-minutes` on the other.
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
- **The paired multicomputer jobs are not co-scheduled — never assume they start
  together.** `multicomputer-master` and `multicomputer-slaves` become eligible
  at the same moment but queue for runners independently, and on a busy pool one
  side has been seen starting **30 minutes** after the other. Both sides must
  therefore tolerate arbitrary skew, and their `timeout-minutes` has to cover
  `peer-wait-minutes` *plus* the ~20-minute suite (hence 55, not the old 30,
  which a healthy run had already been observed using 27 of).

#### Known multicomputer overlay failure modes

Both of these bit on the 7.4.3-alpha4 run and are now mitigated in
`.github/actions/wireguard-overlay`; the symptoms are worth recognising because
neither implicates the code under test.

- **`Timed out waiting for peer endpoint`** — scheduling skew, as above. The wait
  is now a wall-clock window (`peer-wait-minutes`, default 25) instead of a fixed
  120×5s, and it polls the peer's job status so a peer that has *already
  finished* aborts the wait immediately rather than burning the window. Passing
  `peer-job-name` is what enables that; it must match the peer job's `name:`
  exactly, and if it doesn't match (or the jobs API can't be read) the poll
  silently falls back to plain waiting — so a rename degrades the optimisation
  without breaking the tunnel.
- **`[WinError 10013] ... forbidden by its access permissions` from every STUN
  server** — not a network problem. It is `bind()` failing on the Windows master
  because the UDP port sits in the dynamic range (49152+) and WinNAT/Hyper-V had
  reserved it; the bind fails before a packet is sent, so trying more STUN
  servers cannot help. `wg-ports` is now a candidate list defaulting to ports
  *below* 49152, and WireGuard listens on whichever one actually bound. The
  Windows side also dumps `netsh int ipv4 show excludedportrange udp` up front,
  since that evidence is unrecoverable after the fact.

**Jenkins** (`Jenkinsfile`) matrix: platforms ubuntu-noble / debian-trixie /
vs2022 / vs2026; amd64 (plus x86 on debian-trixie); `PACKAGE_TYPE` axis Full
(both MSVC-runtime flavours) and DebugOnly. Stages: Build + Unit Test,
Standalone, Multinode, Multicomputer (cpp only, debian-trixie), Build Examples.

Jenkins' remaining job is a coarse "does enough still work" cross-check against
GHA, guarding against GHA somehow producing a fundamentally different binary. No
Jenkins-built binary is shipped to anyone any more. So partial Jenkins failures
are acceptable evidence, and Jenkins is deliberately *not* kept at feature parity
with GHA: it has no slow-tests stage, and the tests migrated into the TestSuite
component (see Running Tests) therefore run on GHA only. **Decision: no action
taken** — do not add a slow-tests stage to the `Jenkinsfile`.

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
  debug-runtime-only packaging path is unexercised. Adding it will also surface
  the Windows Defender false positive, which currently only fires on that axis
  (see "Windows Defender false positives" above).
- **No 32-bit (x86) on GHA**; Jenkins still keeps debian-trixie × x86. Confirm
  this is an intentional drop, not a silent one.
- **Generic ctest output not archived** — GHA keeps only JUnit XML + the dose
  `dose_test_output`, not `**/test_output/**`.
- **Benign Conan "Cache save failed … another job may be creating this cache"
  annotation** on every build (write-once cache key already saved; harmless but
  noisy, can't be filtered as it's a runner annotation). Fix = save-on-miss-only.

**Newly practical:** a DOPE ODBC test job — hosted runners ship databases
preinstalled, so it's far more tractable than under Jenkins (abandoned there).

### Cutting a Release

Releases are cut by **pushing a version tag**; the `release` job in
`.github/workflows/ci.yml` does the rest. The tag trigger is
`['[0-9]*.[0-9]*.[0-9]*']`, which matches both bare (`7.4.3`) and suffixed
(`7.4.3-alpha4`) versions.

**Manual steps** (all of them; nothing else needs editing for a PATCH/SUFFIX
bump):

1. **`VERSION.txt`** — bump `MAJOR`/`MINOR`/`PATCH`/`SUFFIX`. Use the **dash**
   form for pre-releases (`SUFFIX=-alpha4`); empty `SUFFIX` for a stable
   release. Any API change must bump `MAJOR`, which is the `SOVERSION`.
2. **`build/packaging/debian/changelog`** — add a stanza at the top using the
   **tilde** form (`safir-sdk-core (7.4.3~alpha4-1) UNRELEASED; urgency=medium`).
   Debian needs `~` so pre-releases sort before the stable version;
   `DebianPackager.build` in `build/safir_build_common.py` is the single place
   that translates dash → tilde, and this file must match it.
3. **`CHANGES.txt`** — for a stable release, add the release notes section
   (date, summary, list of fixed issues). Alphas have not carried one.
4. Commit, then `git tag <version>` and `git push origin <version>`. **The tag
   must point at the bump commit itself.** `read_version()` in
   `build/safir_build_common.py` appends a `git describe` hash to the version
   for pre-release `SUFFIX`es *unless* HEAD sits exactly on a tag — that check
   is what keeps the hash out of release artifact names (e.g. the Windows
   installer filename). Tag a later commit and every asset gets a dirty
   `7.4.3-alpha4-...-g<sha>` name.

**Push the tag on its own, not together with the branch.** The tag push carries
the commits anyway, so `git push origin <branch> <tag>` gains nothing and starts
*two* full matrix runs — the `concurrency` group is keyed on `github.ref`, so a
branch ref and a tag ref never share it. That happened on 7.4.3-alpha4 and the
two runs starved each other of runners: paired multicomputer jobs ended up
starting 30 minutes apart, and four of them failed in the overlay rendezvous.
Push the branch separately once the tag run has the runners it needs.

The `installcligac` caveat in `VERSION.txt`'s comments only bites on a
**MAJOR.MINOR** bump — those `Policy.7.4.*` filenames encode MAJOR.MINOR only,
so a PATCH or SUFFIX change leaves `build/packaging/debian/*.installcligac`
alone.

**What the `release` job then does automatically:** builds the full matrix,
bundles each Linux platform's `.deb` set into one
`safir-sdk-core_<ver>_<arch>-<distro>.debs.tar.bz2` (translating the `.deb`
tilde back to a dash and dropping the `-1` debian revision, so asset names match
the old manual releases), copies the Windows `.exe` installers as-is, and
creates the GitHub release with `--generate-notes`. A tag containing a `-`
automatically gets `--prerelease`.

**Two things it deliberately does not do:**
- **It creates a *draft*.** Publishing is a manual click in the GitHub UI, after
  reviewing assets and generated notes. A re-run of an existing release just
  re-uploads assets with `--clobber`.
- **It does not wait for tests.** The job is `needs: build` only, so assets are
  cut as soon as packaging succeeds. Check the test jobs yourself before
  publishing the draft, or add test gating to `needs`.

**Known deltas from the pre-GHA manual releases:** GHA publishes arm64
ubuntu-noble `.deb`s (Jenkins could not build them) but **no x86
debian-trixie** `.deb`s — see "No 32-bit (x86) on GHA" above. There is also no
NuGet publishing step in CI.

**Conventions observed so far, in case they matter:** the 7.4.3 alphas were
tagged off a private feature branch, not `master`, so tagging off `master` is
*not* an established rule. Note also that `7.4.3-alpha1` was tagged locally but
never pushed and has no release — it predates the working automation, so the
first release actually cut this way was `7.4.3-alpha2`.

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

### Windows installer size, and the two things that look wrong but aren't

The VS2022 installer roughly halved between 7.4.2 (333 MB) and 7.4.3-alpha4
(173 MB), unpacked 1989 MB → 1174 MB. Two settled findings, both **Decision: no
action taken**.

**1. Third-party debug info is intentionally absent from the shipped PDBs.**
`CMakeLists.txt` installs conan dependencies with `-s build_type=Release` when
`CMAKE_BUILD_TYPE=RelWithDebInfo` (Safir's own code stays RelWithDebInfo).
Previously ConanCenter had no RelWithDebInfo binaries, so `--build=missing`
built Qt/protobuf/abseil from source *with* debug info, and because they are
statically linked all of it landed in Safir's PDBs — the six Qt GUI app PDBs
alone were 854 MB, now 127 MB. The tradeoff is that Qt/protobuf/abseil frames
in breakpad crash dumps can no longer be symbolized. We are not in the business
of debugging third-party libraries, so this is accepted, not a regression.
Verify with `llvm-pdbutil dump --modules <pdb> | grep -oE 'objects-[A-Za-z]+'`
— `objects-Release` is correct, `objects-RelWithDebInfo` means the split broke.

**2. `icuuc.dll` in a Qt binary's imports is not a missing dependency.**
Qt ≥ 6.9 (7.4.2 used 6.10.1) links the ICU that ships *in Windows* — 1703 added
`icuuc.dll`/`icuin.dll` as system DLLs and 1903 only *added* the combined
`icu.dll` beside them, it did not replace them. So nothing needs bundling, and
`find <installer> -iname '*icu*'` correctly returns nothing. The only
consequence is a Windows 10 1703+ floor. Qt is currently pinned to the 6.8 LTS
(`qt/[>=6.8 <6.9]`) which does not import ICU at all; if that pin is ever
raised, expect the import to reappear — and expect ~3.6 MB per GUI binary of
`qtimezonelocale.cpp.obj` CLDR tables to come back with it, which is where the
6 × 4 MB of `.exe` growth in 7.4.2 came from.

### Debian `-dbg` package size: dwz, not the conan Release split

The `safir-sdk-core-dbg` package shrank between 7.4.2 and 7.4.3-alpha4 (noble
184 → 145 MB, trixie 237 → 216 MB) while every other package grew slightly. The
cause is **`dh_dwz`**, which entered the default debhelper sequence at compat 12
and became active when `build/packaging/debian/compat` (level 10) was deleted in
favour of `debhelper-compat (= 13)` in `debian/control`. dwz dedups DWARF across
binaries into a shared multifile. **Decision: no action taken — keep it on.**

Do *not* attribute this to the Windows conan `-s build_type=Release` mechanism
above; that has no measurable effect on the Debian packages. `debian/rules`
clears `CFLAGS` and `CPPFLAGS` but not `CXXFLAGS`, so `-g` still reaches
dependency builds, and protobuf compile units with full DWARF are present in
both versions. Qt on Linux is the distro's shared Qt6, so the Qt-specific
Windows findings never applied here at all.

Verify dwz is doing its job:
```
readelf -S <file>.debug | grep gnu_debugaltlink        # present => dwz ran
ls usr/lib/debug/.dwz/*/                               # the shared multifiles
readelf --debug-dump=info <file>.debug | grep -c DW_TAG_partial_unit
```
Partial units are dwz output; a count of 0 alongside a missing
`.gnu_debugaltlink` means dwz silently stopped running.

Two consequences worth knowing. The per-binary `.debug` files are now useless
without `/usr/lib/debug/.dwz/…`, so a single `.debug` file cherry-picked out of
the package has a broken symbol table — ship or copy the whole `-dbg` package.
And the gain is much smaller on newer gcc: dwz only touches
`.debug_info`/`.debug_abbrev`/`.debug_str`, and `.debug_line`/`_loclists`/
`_rnglists` grew 22–47% on trixie (gcc 14) versus 5–10% on noble (gcc 13),
partly from the C++17 → C++20 move in `CMakeLists.txt`. Expect the reduction to
keep eroding as the toolchain advances; that is normal, not a broken build.

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
