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

# More verbose (verbosity >= 2 also sets cmake's VERBOSE=1)
build/build.py --verbose --verbose

# Debug build. This is what CI's build-debug job runs; it turns on asserts,
# LeveledLock's lock-order checking and _GLIBCXX_ASSERTIONS.
build/build.py --config Debug

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
**Never pass `-j` to ctest.** The suite cannot run concurrently: tests reuse
singleton resources across processes, so parallel jobs collide rather than
interleave. The logging/tracer/swreport tests all bind one syslog receiver port
and the Dob tests share the type-system shared memory under `/dev/shm/SAFIR_*`;
other cases share fixed ports, `SAFIR_INSTANCE` numbers and temp directories.
`ctest -j2` is already enough to produce message-count mismatches and "no such
type or member defined" failures that have nothing to do with the code, and they
read as real bugs. Nothing in CMake enforces this yet — no test carries
`RUN_SERIAL` or `RESOURCE_LOCK` — so it is on whoever runs the suite.

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

**Writing a dose testcase: wait, don't sleep.** A `Sleep` in a testcase is not
just slow, it is a *deadline* — the `Reset` that ends a testcase closes the
connection and abandons anything still in flight, so whatever the sleep was
covering has exactly that long to happen. That is what made `215-huge_service`
the most frequent flake in CI. Use the `WaitForCallback` action instead: it names
a callback and an occurrence number and blocks the sequencer until the partner
sees it, with a 60 s backstop. It is partner-scoped, so give it a `<Partner>` and
no `<Consumer>`, and count occurrences per testcase, not per phase. Sleeps that
exist to prove *nothing further* arrives are the legitimate remaining use. See
TEST_STATUS.md → "Waiting instead of sleeping" for the semantics and the reasons
they are what they are.

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

### Sanitizer builds (ASan + UBSan)

`-DSAFIR_SANITIZER=address,undefined` (or `thread`; the value goes straight to
`-fsanitize=`) is a cache option in `src/cmake/SafirCompilerSettings.cmake`. It is
for a plain cmake/ninja tree, not `build.py` — a sanitized `.deb` is pointless:

```bash
SAFIR_DONT_BUILD_JAVA=1 cmake -G Ninja -DCMAKE_BUILD_TYPE=RelWithDebInfo \
      -DSAFIR_SANITIZER=address,undefined -DCMAKE_INSTALL_PREFIX=$HOME/install-asan <source>
ninja                                        # SAFIR_DONT_BUILD_JAVA: see the Java note below
ASAN_OPTIONS=detect_container_overflow=0:verify_asan_link_order=0 \
      UBSAN_OPTIONS=print_stacktrace=1:halt_on_error=1 \
      ctest -E java --output-on-failure        # sequentially - never -j
ninja install
export PATH=$HOME/install-asan/bin:$PATH LD_LIBRARY_PATH=$HOME/install-asan/lib
export SAFIR_TEST_CONFIG_OVERRIDE=\
$HOME/install-asan/share/doc/safir-sdk-core/example_configuration   # see below
ASAN_OPTIONS=detect_container_overflow=0:verify_asan_link_order=0 \
      UBSAN_OPTIONS=print_stacktrace=1:halt_on_error=0 \
      run_dose_tests --no-java                 # halt_on_error=0: see the bullet below
ASAN_OPTIONS=detect_container_overflow=0:verify_asan_link_order=0 \
      UBSAN_OPTIONS=print_stacktrace=1:halt_on_error=0 \
      run_slow_tests
```

**The dose and slow suites expect to be run from an installed package**, which is
what normally puts a configuration on the machine. A `cmake --install` into a private
prefix does not: it installs no `/etc/safir-sdk-core`, and `ConfigReader` looks only
in `/etc/safir-sdk-core` and `~/.config/safir-sdk-core`, never under the install
prefix. Use `SAFIR_TEST_CONFIG_OVERRIDE` to run them from a private prefix:

```bash
export SAFIR_TEST_CONFIG_OVERRIDE=\
$HOME/install-asan/share/doc/safir-sdk-core/example_configuration
```

Without it, any driver that does not set the variable itself dies in `ConfigReader`
with `Failed to load configuration` — an abort (`return code -6`) or a caught
exception that looks exactly like a sanitizer finding while having nothing to do with
sanitizers. Of the slow suite's twelve drivers exactly two are in that position, and
the export fixes both: `run_communication_tests` (all 8 suites) and
`run_election_handler_tests` (all cases), verified under ASan+UBSan with zero reports
and nothing written outside the run.

Do **not** instead copy the example config into `~/.config/safir-sdk-core`. It works,
but it is a global fallback picked up by *every* Safir process the account runs, so it
silently changes unrelated runs later with nothing in the repo to explain why. The env
var is scoped to the shell you run the suite in.

Every other driver — system_picture, light_nodes, dope, restart_nodes,
incarnation_and_control, lowmem, tracer_backdoor, and `run_dose_tests` too — sets
`SAFIR_TEST_CONFIG_OVERRIDE` itself, to its own `test_data/<name>/test_config`, plus
`SAFIR_TEST_SUITE_DOU_DIRECTORY` to the installed `share/safir-sdk-core/dou`. Those
override anything you export and are unaffected either way, so if one of them fails,
config is not the reason — check that both of those installed paths exist and then
look elsewhere.

Run the three suites one at a time and never overlapping — the same shared ports,
`/dev/shm` names and `SAFIR_INSTANCE` numbers that make ctest serial-only apply
across suites too. Two of them at once produces a flood of unrelated-looking
failures (`CTRL: Exiting due to error!` from every `safir_control`). Worth knowing
if you drive this from a script: killing a wedged *stage* does not kill the driver
script, which cheerfully moves on to its next stage, so a suite you thought you had
stopped can still be running an hour later. Check before starting anything — but
check by executable, not by command line, for the reason under "Orphaned processes"
below. `pgrep -f install-asan/bin/` will tell you the machine is idle when it is not.

The first full run (2026-09-22) found and fixed a use-after-free in the Linux
`ProcessMonitor` (posted handler captured an iterator into a local `std::set`), a
dangling pointer in the DOU parser's `ParseKey` (`StringToHash` returns a pointer
into the string it is given, and it was given a `substr` temporary), uninitialised
`keyType`/`memberType`/`typeId` fields in the local type descriptions that were
copied into shared memory and read back for every parameter, and a `shared_ptr`
cycle in `ControlCmdSender::SendCmd` that leaked a timer and two callbacks per
command sent. A second run (2026-09-23, rebased onto `develop`) added a misaligned
load in `DataReceiver::ValidCrc`: the crc sits at the end of the datagram, so its
offset is the message length, which is not a multiple of 4 for every message — it
is read with `memcpy` now. That one only fires for messages of the wrong length,
so expect findings of this kind to come and go between runs rather than reproduce
on demand.

Running the *dose* suite under sanitizers then found a fifth, which ctest never
reaches: `PendingRegistration`'s service-registration constructor was the only one
of the three that left `isInjectionHandler` unset, and the copy constructor reads
it. These objects live in shared memory, so the load picked up whatever was in the
page — UBSan reported `load of value 16, which is not a valid value for type
'bool'`. The lesson is that each suite reaches code the others do not; a green
ctest says nothing about the dose or slow suites.

The *slow* suite then found a sixth, in test code: the receive callback in
`system_picture_component_test_node.cpp` validated the delivered buffer and
returned without freeing it, leaking one buffer per message received. Communication
hands ownership over when it delivers — `DeliveryHandler` drops its own reference as
soon as the delivery is posted (`rd.Clear()`, "release reference to data") and only
cleans up what it still holds in `Stop()`, so the receiver must free through the
deallocator it registered. The existing receivers show the idioms: `MessageHandler`
calls `DistributionData::DropReference`, `RemoteSubscriber` wraps the pointer in a
`SharedConstCharArray`, and the `regression_test`/`communication_test` receivers
`delete[]` it directly. If you write a new `SetDataReceiver` callback, free the
buffer.

The same mistake turned out to exist in shipping code, and is the seventh finding:
`StopHandler`'s *stop notification* receiver (`StopHandler.h`) took the buffer,
ignored it because the notification carries no payload worth reading, and returned
without freeing it — one byte leaked per notification, in `safir_control` on every
node. Its neighbour twenty lines up, the stop *order* receiver, gets this right by
wrapping the pointer in a `SharedConstCharArray`; the notification receiver now does
the same. It only shows up in suites that stop nodes and then check their exit codes,
which is why ctest and the dose suite never saw it.

**Changing who frees the buffer is a change to the unit tests too.** The mocks that
drive these receivers do not implement the deallocator — `StopHandler_test`'s mock
Communication just stores the callback and calls it — so the buffer a test hands over
has to be allocated the way the real deallocator expects. Making the notification
receiver free its buffer is what proved this: a `static const char[1]` handed to it —
which is what you reach for when you think nobody will free it — then goes through
`delete[]`, and the test dies inside the free. Run `ctest -R <the handler>_test`
after touching a receiver; the slow suite will not tell you about this.

Where the slow suite stands under sanitizers (2026-09-24, this machine): with the
leaks above fixed and `SAFIR_TEST_CONFIG_OVERRIDE` exported, eleven of the twelve
drivers pass with zero sanitizer reports — `run_system_picture_component_tests` at 52
cases, `run_incarnation_and_control_tests` at 1, `run_light_nodes_keep_state_tests` at
4, `run_light_nodes_clear_state_tests` and `run_light_nodes_smart_sync_tests` at 6
each, plus `run_communication_tests`, `run_election_handler_tests`,
`run_lowmem_basic_operations_tests`, both dope backends and `run_tracer_backdoor_tests`.

`run_restart_nodes_tests` is the one that does not finish, and it is a capacity
problem rather than a defect: it brings up 11 nodes, an instrumented `dose_main` is
about 0.7 GB RSS, and on a 15 GB 4-core box that means ~14 GB used, under 2 GB
available and a load average around 7. It sits on `dose_main is waiting for
persistence data` and makes no progress; it emits **no sanitizer report** before
timing out. It passes in 589 s on a plain Release build of the same tree, so there is
nothing to chase in the code — either run it on a bigger machine or don't run it
under sanitizers.

Things that look like findings but are not, and how the run is set up to avoid them:

- **Java cannot host ASan.** The JVM maps its heap where ASan's shadow memory has
  to go, so a Java process that loads a sanitized JNI library aborts with `Shadow
  memory range interleaves with an existing memory mapping` before any test code
  runs. Configure with `SAFIR_DONT_BUILD_JAVA=1` (or `ctest -E java`); this is not
  fixable from our side.
- **.NET needs `verify_asan_link_order=0`, and then works.** `mono` is an
  uninstrumented host that `dlopen`s our instrumented libraries, so ASan complains
  that its runtime "does not come first in initial library list" and every
  `*_dotnet` test fails at startup. Unlike the JVM there is no shadow-memory
  conflict, so disabling the check is enough — all six `*_dotnet` tests pass. Leave
  this option out of `ASAN_OPTIONS` and you get six failures that look like real
  breakage but are pure link order.
- **Parallel ctest failures.** Not a sanitizer effect at all — the suite is
  serial-only in every build; see "Running Tests" above.
- **Orphaned processes from an earlier run, and this is the expensive one.** A driver
  that times out or fails leaves nodes behind — `run_restart_nodes_tests` timing out
  under sanitizers orphaned 11 `dose_main` and 2 `safir_control`. They keep holding
  their `SAFIR_INSTANCE`, so the next driver that reuses that instance number cannot
  initialise, and if you clear `/dev/shm` while they are alive you delete the named
  semaphore they created. That produces

  ```
  It appears that Create failed in some other process for 'SAFIR_DOTS_INITIALIZATION_<n>'
  ```

  in the *new* process, which surfaces as `safir_control`/`safir_web` exiting with
  code 20 and, for the light-node drivers, a refused websocket on
  `ws://localhost:16675`. It looks like a broken build. It is not.

  **Do not clean up with `pkill -f '<prefix>/bin/'`.** Several Safir processes are
  launched with a bare `argv[0]` — `safir_web`, `RequestSender`,
  `WaitingStatesOwner` — so a pattern anchored on the install path never matches
  them and they survive every cleanup while looking absent to `pgrep`. Match on the
  executable instead, which catches them however they were invoked and cannot match
  the shell doing the killing:

  ```bash
  for p in /proc/[0-9]*; do
      case "$(readlink $p/exe 2>/dev/null)" in "$PREFIX"*) kill "${p#/proc/}";; esac
  done
  ```

  Kill first, verify nothing is left, and only then clear `/dev/shm` and the lock
  directory. This cost a full day once: 18 `RequestSender` processes from a failed
  restart_nodes run stayed alive for 20 hours holding instances 2..10, and made five
  drivers look permanently broken under sanitizers when all five were fine.
- **When in doubt, build the same tree without sanitizers.** A plain
  `-DCMAKE_BUILD_TYPE=Release` tree and a second install prefix costs about 35 min of
  build on a 4-core box and 128 MB on disk, and it separates "the sanitizer found
  something" from "this environment cannot run this suite" in one run. Worth it before
  spending hours on a suspected finding.
- **`detect_container_overflow=0`** is needed because the statically linked Conan
  Boost is uninstrumented and mixes with instrumented code on the same containers,
  a known false-positive source.
- **UBSan reports and continues by default, and for the multi-process suites it
  should stay that way.** `halt_on_error=1` is right for ctest, where stopping at
  the first finding is what you want. It is actively harmful for the dose and slow
  suites: a partner that aborts at its own report leaves `dose_test_sequencer`
  waiting forever on a peer that will never reply, so the run hangs with no
  diagnosis and everything after the first finding goes unexplored. That is exactly
  what happened on the first dose run under sanitizers — four "Reading reply
  failed:End of file" lines, then nothing, for a single uninitialised `bool`. Use
  `halt_on_error=0` there and collect the findings afterwards.
- **In the slow suite one leak fails everything.** Each system-picture component
  test decides its verdict as literally `node.returncode == 0`, and a sanitizer that
  reports at exit sets that returncode non-zero. So the single leaked buffer above
  failed all 43 cases in `run_system_picture_component_tests` — 43 red tests, one
  cause, and none of them a timing flake. Before chasing slow-suite failures under
  sanitizers, check whether the nodes merely exited non-zero on a report:
  `grep -c "exited with error code" <log>` against the report files. Conversely a
  leak anywhere in a node hides every real failure behind it, which is why it is
  worth fixing test-code leaks rather than suppressing them.
- **Where the reports actually land.** `log_path=<dir>/x` in `ASAN_OPTIONS` and
  `UBSAN_OPTIONS` writes one report file per process, which is how to see anything
  from `dose_main`/`dope_main` children whose stderr a driver swallows. It is not
  the whole story for the dose suite: `run_dose_tests` already redirects each
  partner's stdout and stderr to `dose_test_output/<name>.output.txt`, and that is
  where the partner reports turned up — the `log_path` directory stayed empty for
  them. Check both, and grep the `.output.txt` files for `runtime error` rather
  than trusting a green-looking driver.
- **The deliberate-crash tests** (`CrashReporter_*`, `DynamicLibraryLoader`) pass
  `handle_segv=0:handle_sigfpe=0:handle_sigill=0:handle_abort=0` to their children
  themselves, so the child dies of the signal the test expects instead of ASan
  turning it into exit code 1. `simple_crash_test` additionally forces
  `halt_on_error=0` for its child, because the crasher provokes its signals with
  deliberate undefined behaviour (a null store, a division by zero) and UBSan would
  otherwise abort at its own report *before* the signal under test is raised — the
  symptom is `CrashReporter did not call callback!`. Nothing to do when adding a
  sanitizer job.
- **A sanitizer abort mid-test leaves `safir_control`/`dose_main`/`dope_main`
  running**, and they keep `/dev/shm/SAFIR_*` alive with whatever DOU set that test
  had loaded. Every later Dob test then fails with `There is no such type or member
  defined`, the ExternalTimeProvider tests return the wrong time, and
  `start_fails_with_parser_errors` sees a configuration check succeed. That is
  stale state, not a bug: kill the leftovers and remove `/dev/shm/SAFIR_*` and
  `/dev/shm/sem.*SAFIR*` before re-running.

There is no sanitizer CI job yet. If one is added, model it on `build-debug` but
drive cmake/ninja/ctest directly, exclude Java, and run the all-C++ dose combination.
On hosted `ubuntu-24.04` runners TSan (and sometimes ASan) needs
`sudo sysctl vm.mmap_rnd_bits=28` first.

### Windows Defender false positives

Since 2026-08-17 Defender flags **Debug-built** `safir_control.exe` as
`Exploit:Win64/Facupel!dha`. `CreateProcess` then refuses to start the image, so
`TryStart_safir` fails with `OSError [WinError 225]` (`ERROR_VIRUS_INFECTED`).
The binary never runs, so this is not a code fault.

**Scope: Windows Debug builds only** — the sole configuration that builds a
Debug `safir_control.exe` and runs ctest against it. That configuration is not
built anywhere today: in a Windows packaging build the Debug pass builds only the
`safir_dual_abi_libs` target and skips tests altogether, so the package ships
RelWithDebInfo executables plus debug dual-ABI *libraries*, never debug
executables. The published 7.4.2 installer was checked and is clean, so **users
are unaffected**, and CI is unaffected because its Debug job is Linux-only. This
is the blocker anyone adding a Windows Debug row has to solve first — see "Debug
coverage" under CI/CD.

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
priority, since nothing currently builds the affected configuration. If Microsoft
revises the rule, the detection simply stops firing after a definitions update —
build Windows with `--configs Debug` to confirm. No workaround was applied;
`Add-MpPreference -ExclusionPath <workspace>` is the obvious lever if a Windows
Debug job is ever added (hosted Windows runners are elevated, so it should be
available — untested). To re-settle this
if it recurs, rebuild an older tag whose release build was clean: the same
sources flagged today means Defender changed, not us. Note that nothing in CI
disables Defender antivirus — the `netsh advfirewall` call in the multicomputer
jobs turns off the Defender *firewall*, a different component.

### CI/CD

**GitHub Actions** (`.github/workflows/ci.yml`) is the only CI system. Jenkins
was retired in 7.4.4 (#615) and every trace of it has been removed from the
tree; if you find a stale reference, delete it rather than restoring anything.

The workflow runs on pushes to master/develop/feature/private branches and on
pull requests. A matrix builds
and packages across ubuntu-noble (amd64 + arm64), debian-trixie, vs2022 and
vs2026; there is no Debian-labelled runner, so debian-trixie builds inside a
`debian:13` container on an ubuntu-24.04 host (with `--shm-size`, because dose_main
needs a 100 MB `/dev/shm`).

Every runner here is GitHub-hosted; this project has no self-hosted runners. In
particular **`windows-2025-vs2026` is a normal GitHub-hosted image**, despite the
unusual-looking label — do not read it as self-hosted, and do not "simplify" it
to a plain `windows-2025`: the suffixed label is what selects the image carrying
the VS2026 toolchain.

Each row runs `build/build.py --verbose`; downstream
jobs install the package and run the example builds, the dose test suites
(standalone, multinode, and multicomputer — the last joins two runners over an
accountless WireGuard overlay so a native node talks to three debian slave
containers), and the installed slow-test suite. Alongside those,
`build-debug` / `debug-dose-tests` / `debug-slow-tests` do the same on
ubuntu-noble amd64 in **Debug** — see "Debug coverage" below. A `test-summary`
job aggregates JUnit results into one Check, a `release` job drafts a release on
version-tag pushes, `render-docs` renders the guides, and `workflow-lint` runs
zizmor.

Every test job also uploads its raw output as a `*-output-*` artifact, alongside
the junit: `**/test_output/**` from the build, build-debug and slow-test jobs
(every ctest/slow driver that keeps logs writes to a directory named exactly
`test_output`), and `dose_test_output` (the per-partner `*.output.txt`) plus
`temp` (`$SAFIR_TEST_TEMP`: lock files, logs, crash dumps) from the dose and
multicomputer jobs. The junit tells you which case failed; these tell you why.
The name is deliberately outside the `*results-*` glob that `test-summary`
downloads. This restored what the retired Jenkins pipeline archived as
`*.output.txt` and `test-output.zip`.

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
  them. The policy is **a wall-clock budget, or 13 attempts, whichever runs out
  first**, spaced 30s doubling to a 15-minute cap. The budget is wall clock
  rather than the sum of the sleeps because a hung install costs whatever its own
  timeout is, which dwarfs the sleeps; bounding the sleeps let run 33111295598
  spend six hours in one setup step.
- **THE INVARIANT: a job's retry budget must be smaller than its
  `timeout-minutes`, with room for the work the job still has to do.** Break it
  and the give-up path is unreachable — the job is killed mid-retry, and GitHub
  reports that as **cancelled**, which reads like somebody pressed a button and
  hides the real cause. Run 33176126427 lost a dose leg exactly that way: a 2-hour
  budget inside a 60-minute job, retrying a `packages.microsoft.com` 403 until the
  clock ran out. **If you change a `timeout-minutes`, check this.** The budget is
  therefore per context, set through `RETRY_BUDGET_SECONDS`: 2700s (45 min) by
  default, which fits every test job, and 7200s in `setup-build-env` because
  `build` gates the whole matrix and waiting there can save re-running everything
  downstream. `build` and `build-examples` carry explicit `timeout-minutes` for
  no other reason than to keep the inequality true — without one they inherit
  GitHub's 6-hour default and nothing holds them to it.
- **Do not let `apt-get update` fail on repos we never install from.** It fails
  the whole command if *any* configured repo is unreachable, and the hosted Ubuntu
  images ship `azure-cli` and `microsoft-prod` source lists that have nothing to do
  with this build. Both setup actions delete any source list mentioning
  `packages.microsoft.com` before updating, matched by content rather than
  filename because the images rename them.
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
- **Pin the Ubuntu release wherever the host OS matters; `ubuntu-latest` only
  where it does not.** Pinned to `ubuntu-24.04`: the native noble jobs, the
  debian-trixie container rows (they still get the host's kernel, Docker and
  `/dev/shm`), `render-docs` (installs the docs toolchain from the host's apt,
  so a new release can silently change the rendered output) and
  `multicomputer-slaves` (host apt, `modprobe wireguard`, iptables and a Docker
  bridge). `workflow-lint`, `test-summary`, `warnings-summary` and `release`
  stay on `ubuntu-latest` **deliberately**: they only run actions and `gh`, so
  pinning them would only add a label to bump by hand. Decided when GitHub
  announced `ubuntu-latest` moving to Ubuntu 26 from 2026-10-19; when 24.04 is
  retired, bump the pinned jobs together.
- **The paired multicomputer jobs are not co-scheduled — never assume they start
  together.** `multicomputer-master` and `multicomputer-slaves` become eligible
  at the same moment but queue for runners independently, and on a busy pool one
  side has been seen starting **30 minutes** after the other. Both sides must
  therefore tolerate arbitrary skew, and their `timeout-minutes` has to cover
  `peer-wait-minutes` *plus* the ~20-minute suite (hence 55, not the old 30,
  which a healthy run had already been observed using 27 of).

#### Known multicomputer overlay failure modes

All three of these are mitigated in `.github/actions/wireguard-overlay`; the
symptoms are worth recognising because none of them implicates the code under
test. The first two bit on the 7.4.3-alpha4 run.

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
- **Both sides report `0 B received` and time out waiting for a handshake, on a
  *re-run*** — the rendezvous artifacts are scoped to the run **id**, which
  `gh run rerun` does not change, so a re-run used to publish a second
  `endpoint-<role>` artifact next to the first. The fetch picked between two
  same-named artifacts with `head -n1` and could get the previous attempt's,
  whose runner no longer exists; both sides then configured a dead peer and sent
  handshakes into the void. Seen on run 35312741704, where `gh run rerun --failed`
  of the multicomputer pair failed this way symmetrically. The artifact name now
  carries `github.run_attempt`, so an attempt cannot see an earlier one's
  endpoints. **Before that fix, `gh run rerun --failed` was simply unsound for
  the multicomputer pair** — the only reliable re-test was a fresh run. It is
  sound now **only when both halves of a pair failed**, which is the normal case
  since they fail together. If just one half failed, its re-run has no peer: the
  jobs API lists the other half as already `completed` (verified on the attempt-2
  job list of run 35312741704), so the poll gives up within seconds with "has
  already completed without publishing an endpoint" rather than hanging — a fast
  and clear failure, but the only way to re-test that pair is to re-run the whole
  workflow.

#### Debug coverage (`build-debug` and friends)

Every other job builds RelWithDebInfo, which defines `NDEBUG`. Three things
vanish under it, so **without a Debug job they run nowhere at all**:

- the ~50 `assert()` calls in the source tree;
- `LeveledLock`'s lock-**order** checking. The deadlock detector is
  `#if !defined(NDEBUG) && !defined(DOSE_NO_LOCK_CHECKING)`, so it is absent from
  every binary we otherwise build, test or ship;
- the `NDEBUG`-guarded cases in `leveled_lock_test` (about half the file) and
  `ElectionHandler_test`.

Jenkins' `PACKAGE_TYPE = DebugOnly` axis covered this until it was retired.
Nothing covered it in between, so the Debug jobs are a **restoration**, not a new
idea. If you are tempted to delete them to save CI time, that is the coverage you
are deleting.

**Decisions worth not relitigating:**

- **ubuntu-noble amd64 only, on purpose.** Debug is built and tested on exactly
  one row. There is no Debug coverage on Windows, on arm64 or on debian-trixie,
  and that is a decision, not a gap waiting to be closed: what Debug adds is the
  `assert()`s, the lock-order checking and `_GLIBCXX_ASSERTIONS`, and all of
  that is platform-neutral code that behaves the same on every row. A second
  Linux row (arm64 or debian's newer GCC/libstdc++) would re-run the same checks
  against the same code for another ~35 minutes of runner time, with nothing we
  believe is significant to gain. A Windows row would add only MSVC's checked
  iterators — every `assert()` is in platform-neutral code; the three files
  matching both `assert(` and `_MSC_VER` only do so because of
  `#pragma warning(push/pop)` — and it would first have to get past the Defender
  false positive that eats Debug `safir_control.exe` (above). Chase that class of
  bug with a sanitiser job instead.
- **`_GLIBCXX_ASSERTIONS`** is set for GNU/Clang Debug builds in
  `src/cmake/SafirCompilerSettings.cmake`. It is ABI-safe, so a Debug build still
  links against the release-built Conan Boost; `_GLIBCXX_DEBUG` is **not** and
  would break that. It is written without a value to match dpkg-buildflags' form,
  so a duplicate definition is identical rather than a mismatch warning.
- **Set it explicitly; do not rely on libstdc++ turning it on for you.**
  `c++config.h` self-enables assertions when `__OPTIMIZE__` is undefined, which
  makes it tempting to assume any Debug build gets them. It is not safe to assume:
  CMake's `CMAKE_CXX_FLAGS_DEBUG` is `-g` alone, *not* `-g -O0`, so whether
  `__OPTIMIZE__` ends up defined is left to the compiler's default and to whatever
  the distribution's wrapper injects. On a plain `-g` build this was observed
  coming out *defined*, silently disabling every check. #616 was found by these
  assertions, and reproduced only when the define was passed explicitly — with it
  absent the buggy code passed the test.
- Empirically the packaging build does **not** already have it: in CI run
  35230386248 the RelWithDebInfo ubuntu-noble and debian-trixie rows passed
  `dots_parser_test_cases` while the Debug row failed it, so `dpkg-buildflags`
  does not set `_GLIBCXX_ASSERTIONS`.
- **The dose rows run `java-cpp-dotnet-java-cpp`, not the all-cpp combo, and this
  is load-bearing.** `dose_java_jni` has no `ADD_TEST` anywhere, so its 27 asserts
  — over half of all the asserts in the tree — are reachable *only* from a Java
  partner in the dose suite. The dotnet interop has no asserts at all.
- **Debug slowness is fixed with config-conditional constants**, never by
  loosening a timeout globally. See `ElectionHandler_test.cpp`'s `numNodes` or
  `ControlApp`'s `#if defined(_MSC_VER) && !defined(NDEBUG)` termination timeout
  for the established pattern: Release keeps its tight values.
- The Debug build log is uploaded as `debug-buildlog-*`, deliberately **not**
  `buildlog-*`: `warnings-summary` globs the latter, and this is a `noopt` build
  whose warning set differs systematically from the optimised ones.
- **All three Debug jobs hard-fail on any test failure** via
  `build/ci/check_junit.py`, breaking the workflow's usual rule that a test
  failure only reddens the "Test results" check. This is deliberate — a failure
  here is a bug class nothing else in CI can see — but it is **provisional**, and
  it pulls against the flakiness-deferral policy, since these jobs run the same
  flake-prone dose and slow suites as everything else. TEST_STATUS.md ("The Debug
  jobs opt out of all of that") records the tradeoff and the ordered list of ways
  to soften it. Soften it rather than sinking time into flake research.

**Known gap:** Debug is exercised on ubuntu-noble amd64 only. On Windows that
leaves MSVC's checked iterators and debug CRT heap unexercised; on arm64 and
debian-trixie it leaves a second compiler/libstdc++ unexercised under asserts.
Accepted, per the reasoning above.

**Newly practical:** a DOPE ODBC test job — hosted runners ship databases
preinstalled, so it's far more tractable than it used to be (abandoned
pre-migration).

#### Known gaps and accepted drops

These outlived the Jenkins migration and are still true. They are recorded so
nobody re-discovers them as bugs.

- **No build-warnings quality gate.** `warnings-summary` reports but does not
  gate (GitHub has no "unstable" state); enforce later via a `quality-gates`
  block on the Quality Monitor job.
- **No 32-bit (x86) anywhere.** This is a **confirmed intentional drop**, not an
  oversight — decided 2026-09 and announced in the 7.4.3 release notes, so users
  are told rather than left to discover it. 7.4.2 was the last release to ship an
  x86 `.deb`. It could be picked up again, but that is considered unlikely; do
  not treat it as a gap to be closed. (32-bit *Windows* is a separate, earlier
  drop — #560 in 7.4.1.)
- **Debug testing on ubuntu-noble amd64 only** — none on Windows, arm64 or
  debian-trixie, by design. See "Debug coverage" above.
- **Benign Conan "Cache save failed … another job may be creating this cache"
  annotation** on every build (write-once cache key already saved; harmless but
  noisy, can't be filtered as it's a runner annotation). Fix = save-on-miss-only.

### Git and Branch History

**Keep `develop` linear. Do not create merge commits.** A feature branch lands
as its commits placed directly on top of `develop` — fast-forward, rebase, or a
hand-built branch off the current tip — never behind a `Merge branch '...'`
commit.

This is not written anywhere else, and the log will actively mislead you if you
go looking: `develop` has 260 merge commits among its 6429, so a query like
`git log --merges -5` happily returns five of them and reads like proof that
merging is normal here. It is not. **They are all from 2016-2017; the most
recent is 52a25b9ed, 2017-09-04.** There are none in the last 200 commits. If
you want to check the convention rather than assume it, ask a question that has
dates in it:

```bash
git log --first-parent -200 --format='%h %cs %p %s' develop   # >1 parent = a merge
git log --merges -1 --format='%h %cs %s' develop              # when was the last one
```

**Land a branch as a small number of clear commits, not its working history.**
The development history of a branch — the false starts, the fixes to the fixes,
the "record what CI said" commits — is not what belongs on `develop`. Rewrite it
into commits that each do one intelligible thing and each leave the tree
building. Splitting by theme works well: infrastructure separately from the
change it enables, a new mechanism separately from the code that starts using
it, so a bisect landing between them still compiles.

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
5. **For a stable release, put the `CHANGES.txt` section into the draft's
   release notes.** The `release` job creates the draft with `--generate-notes`,
   which yields nothing but a `**Full Changelog**: <compare link>` line. Replace
   that with the release's own `CHANGES.txt` section, copied verbatim: start at
   the prose, drop the separator/date header, end at the last `#NNN` line, and
   do not keep the generated compare link (7.4.2 and 7.4.3 both look like this).

   ```sh
   gh release edit <version> --notes-file <extracted-section>
   ```

   Do this *before* publishing, while it is still a draft. Verify the working
   tree's `CHANGES.txt` matches the tag first (`git diff <version> --
   CHANGES.txt`) so the notes describe what was actually released rather than a
   later edit. Alphas skip this — they carry no `CHANGES.txt` section at all
   (step 3), so the generated compare link is all they get.
6. **For a stable release, fast-forward `master` onto the tag afterwards.**
   Tags are cut off `develop`; `master` then moves up to *the tag*:

   ```sh
   git push origin <version>:master        # or: git merge --ff-only <version>
   ```

   **Onto the tag, not onto `develop`'s tip.** Those are the same commit only
   until the next-cycle bump lands, which is usually the very next commit on
   `develop` (step 7) — after that, "merge `develop` into `master`" would carry
   an unreleased alpha bump onto `master`. Every stable tag is reachable from
   `master`, and `master`'s tip is exactly the most recent stable release
   commit; keep it that way. Skip this and `master` silently falls a release
   behind. Alphas are not merged to `master`.
7. **Then open the next cycle on `develop`:** bump `PATCH` and set
   `SUFFIX=-alpha1` in `VERSION.txt`, and add the matching `~alpha1` stanza to
   the debian changelog. No `CHANGES.txt` entry — alphas do not carry one.
   Commit as "Prepare for next release cycle". `64e3218cb` is the template, but
   note it predates the current convention and used `SUFFIX=~alpha1`:
   `VERSION.txt` now wants the dash form (`-alpha1`), with the tilde appearing
   only in the debian changelog.

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
ubuntu-noble `.deb`s (the old Jenkins build could not) but **no x86
debian-trixie** `.deb`s — see "Known gaps and accepted drops" above. That is the only
delta.

**NuGet is not a delta, despite appearances.** There is CPack NuGet packaging
in the tree (`src/cmake/NuGetPackaging.cmake`, an `EXCLUDE_FROM_ALL` `NuGet`
install component, `docs/nuget-readme.md`), and no CI step that publishes it —
but the pre-GHA release process never published one either, and no release has
ever carried a `.nupkg`. It is the unfinished start of #554 (Modernize dotnet interfaces,
milestone 7.5), whose own notes lean towards shipping nupkgs *in the installer*
rather than uploading them, so a publishing step may never be the right end
state. Nothing was lost in the migration.

**Conventions, and one that is easy to get backwards:** you never tag `master`
directly — tags are cut off `develop` and `master` is fast-forwarded to them
afterwards (step 6). Alphas are looser: the 7.4.3 alphas were tagged off a
private feature branch and never merged to `master` at all. Note also that
`7.4.3-alpha1` was tagged locally but never pushed and has no release — it
predates the working automation, so the first release actually cut this way was
`7.4.3-alpha2`.

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

### Copyright Headers

Source files carry a `Copyright Saab AB, <years> (http://safirsdkcore.com)` line
in their header comment. **When you make a non-trivial change to a file, add the
current year to that line.** Trivial changes - a typo, reflowing a comment,
whitespace - do not count.

The years are a list of ranges, and the current year is folded into it:

- `2023-2025` becomes `2023-2026` - the range already ends at last year, so
  extend it rather than starting a new entry.
- `2025` becomes `2025-2026`, for the same reason.
- `2003-2018` becomes `2003-2018, 2026` - there is a gap, so append a new entry.
- `2013, 2024` becomes `2013, 2024, 2026`.

Separate entries with a comma and a space. Both `, 2026` and `,2026` occur in
the tree; the spaced form is the more common one and is what new entries should
use. Leave the existing entries in a header alone, however they are punctuated -
this is not a reformatting exercise.

Files with no such header (DOU files, test case XML, CMake, YAML, Markdown) do
not get one added.

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
