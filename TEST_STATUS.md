# Test status & known flaky tests

Companion to [AGENTS.md](AGENTS.md). This file holds the durable, cross-host
record of **which CI tests fail intermittently, why, and how to tell a flake from
a real regression.** It is kept in-repo on purpose so every developer and agent,
on any host, sees the same triage state. When you diagnose a flake (or fix one),
update this file rather than keeping the notes local.

## Policy: defer flakiness unless it's ours

**A single red CI run is usually a known flake, not your change.** The tests below
fail intermittently for reasons unrelated to the GitHub Actions migration. The
standing decision (2026-08-23) is to **defer all such flakiness** and *not* gate
the branch on it; it is catalogued here rather than in issues (see the next
section). The only flakiness worth fixing now is
anything caused by our own workflow/refactoring (a bug in the CI logic, driver
scripts, or test wiring we wrote) — pre-existing test flakiness that merely now
runs on GHA is not a blocker.

When a run goes red, classify each failure before reacting:
- **Pre-existing test flakiness** (everything catalogued below) → re-run; don't
  treat it as a regression.
- **Workflow/refactoring defect** → fix now.
- **Runner-environment quirk** → usually defer, but name it.

## Where flakiness is tracked: here, not in GitHub issues

**Decided 2026-08-25: this file is the only record of known test flakiness. Do not
open a GitHub issue for a flaky test.** An open issue and an entry here mean
different things — an open issue is a claim that someone intends to do the work,
while an entry here is a statement of a known condition. Keeping both never stays
in sync: the diagnosis ends up in one place and the other rots silently. That is
exactly what happened to #396 and #397, which sat open for four years while the
real triage state accumulated here, and to `Communication_ResetTest`, whose issue
was closed as fixed in June while this file still listed it as flaky in August.

- **Raise an issue when we decide to investigate or fix a specific flake** — then
  the open issue means something. Link it from the entry below.
- **Exception — a real product bug is not flakiness.** If diagnosing a flake
  uncovers a genuine defect in the product, that gets an issue on its own merits,
  scheduled or not; burying a real bug in a file called "known flaky tests" reads
  as won't-fix. Rule of thumb: *the test is unreliable* → this file. *The product
  is wrong* → an issue, linked from here. Both worked examples came out of flake
  investigations and are now filed on their own merits: **#613** (Coordinator
  aborts the node on an inconsistent state) and **#614** (a receive error
  permanently kills a Communication read loop).
- **Closed historical issues:** #396 (`syslog_output` / Boost.Asio latency) and
  #397 (`361-inject_update_and_delete_for_existing_entity`) were closed on
  2026-08-25 under this policy and point here.

## How CI signals a failure

The build/test matrix jobs stay **green** on a test-case failure; the failure is
carried by the JUnit XML and surfaces only as a red **"Test results"** Check
(EnricoMi `publish-unit-test-result-action`, which parses `**/*.junit.xml`). This
matches the slow-suite signalling described in AGENTS.md (a *test-case* failure is
exit 1, carried by junit; an *infra* failure is exit 2 and fails the job).

**One exception turns a job itself red:** `safir_control.0.returncode` (below).
The dose runner treats a nonzero `safir_control` *process* exit as an infra
failure, not a carried assertion, so it fails the whole job.

To audit per-platform outcomes, download the `*-results-*` artifacts from the run
and read the `*.junit.xml`. Note the dose junit `time="0"` is a hardcoded literal
(`src/tests/dose_test.ss/run_dose_tests.py.in:307-308`), never measured — "took
0s" carries no information.

## Known flaky tests

Frequency is rough and drifts. **Last seen** is the most recent run that actually
failed the test, from a scan of the last 40 `ci.yml` runs (2026-06-28 → 2026-08-25)
— so "last seen" older than 2026-06-28 just means "not in the scanned window".
Update it when you see a fresh occurrence; an entry whose last-seen keeps receding
is a candidate for the dormant list.

`215-huge_service` is by far the most common — 10 of the ~25 runs in that window,
so a lone `215-huge_service` red is the single most likely thing you'll see.

`Communication_ResetTest` used to head this list; it was **fixed on 2026-06-30**
and is no longer flaky — see "Fixed / dormant" below before you re-diagnose it.

| Test | Where | Platform | Freq | Last seen | Character |
|---|---|---|---|---|---|
| `215-huge_service` | multicomputer dose (overlay) | any | **high** | 2026-08-24 (32742752685) | Huge round-trip occasionally not delivered |
| `518-huge_entity` | multicomputer dose (overlay) | any | low | 2026-08-20 (32348189223) | Same huge-message family as 215 |
| `syslog_output` | multinode dose | any | low | 2026-08-21 (32461278855) | Canary — fails on any unexpected syslog; read its body |
| `353-pending_entity_handler_registration_between_nodes` | multicomputer dose | any | low | 2026-08-18 (32143455257) | "Pending registration" family |
| `155-pending_service_registration_same_node` | dose | any | low | 2026-08-20 (32348189223) | "Pending registration" family |
| `2007-lightnode_limited_entity_on_normal_node` | multicomputer dose | any | low | 2026-08-21 (32461278855) | Light-node detach/reattach |
| `HeartbeatSenderTest` | slow suite (`run_communication_tests`) | Windows | low | 2026-08-18 (32143455257) | Communication flake, newly visible via slow-suite junit |
| `safir_control.0.returncode` | multinode dose | **Windows** | low | 2026-08-21 (32461278855) | `safir_control` exit 1; **fails the job**; overload→Coordinator crash |
| `run_restart_nodes_tests` (hang) | slow suite | any | low | 2026-08-23 (32637686999) | Hangs at startup → TIMEOUT → **fails the job** |

### Job-level / infra flakes (red job, not a test-case failure)

Some reds are not test failures at all — a job dies before or around the tests, so
the "Test results" Check can still say "all pass" while a job is red. Recognise
these so you don't hunt for a nonexistent test regression:

- **Multicomputer overlay rendezvous** — `multicomputer-master-*` /
  `multicomputer-slaves-*` failing in the "Fetch peer endpoint" step (symptoms:
  "Timed out waiting for peer endpoint", `[WinError 10013] … forbidden by its
  access permissions`, or a rendezvous-artifact `digest-mismatch`). These are
  scheduling-skew / Windows dynamic-port / artifact issues in
  `.github/actions/wireguard-overlay`, **not** the code under test. Full
  diagnosis and mitigations are in **AGENTS.md → "Known multicomputer overlay
  failure modes."** A frequent trigger is two full-matrix runs competing for
  runners (e.g. a tag run and a branch run at once) — see AGENTS.md → "Cutting a
  Release" on pushing the tag on its own. Seen on runs 32348188649 (the
  7.4.3-alpha4 tag) and 32457868482.
- **Not everything red is a flake.** A `slow-tests` job once failed with all
  tests passing (run 31795493619) because of a genuine setup bug (a missing
  `system_picture_listener`), fixed immediately after. If a job dies with no
  test-case failure and it's *not* the overlay rendezvous, read the job log — it
  may be a real regression in the CI wiring, which is exactly the class we do
  fix now.

### `run_restart_nodes_tests` hang (slow suite → TIMEOUT → red job)

Seen on CI #134 (run 32637686999, slow-tests `ubuntu-noble-amd64`). The driver
normally runs ~550 s; here it printed `Launching node 0` and then produced **no
further output for the full 30 minutes** until the umbrella's per-driver timeout
(1800 s) killed it. So it hung **at startup — node 0 never progressed** — rather
than deadlocking mid-test. A TIMEOUT is classified as infra (umbrella exit 2), so
it **reddens the slow-tests job**, not just the "Test results" Check.

The 1800 s timeout is deliberately generous (~3.3× the observed runtime) and did
its job: it killed the hung driver and reported it against its own name instead of
letting the global CI job timeout kill every driver blind — so **don't "fix" this
by shortening or lengthening the timeout.** Root cause not yet isolated (node 0
launched, then silence — a dose_main/safir_control startup or system-formation
hang is the likely area). Pre-existing, environment-tolerant behaviour, not caused
by the migration; defer.

### `215-huge_service` (+ the huge-message family)

Intermittent failure in the **multicomputer** dose suite over the WireGuard
overlay. First seen 2026-08-16 (run 31953726975, `ubuntu-noble-arm64` master):
aggregation reported "1 out of 34 runs failed"; the workflow still concluded
`success`, only the "Test results" Check went red.

**Signature:** the huge `ComplexGlobalService` request never reaches Partner 0 /
Consumer 0 (`OnServiceRequest` missing) and Partner 2 / Consumer 1 never gets the
response (`OnResponse` missing) — the whole `----- Test -----` section is absent on
both partners in the junit diff.

**Why it's a flake, not "huge messages break over the overlay":** every other
large-payload test passed on the *same* run/overlay — `009-huge_message`,
`518-huge_entity` (equally huge), `008-complex_message`, `214-complex_service`
(the non-huge sibling of 215), `517-complex_entity`. `215-huge_service` is unique
as the largest, most timing-sensitive round-trip (huge fragmented request *plus* a
response leg), so it's the one most likely to lose a fragment or miss its window on
a single bad run. Dob node comms are UDP unicast.

**If it recurs:** look at the huge-service round-trip retransmit/timeout over the
overlay. Otherwise a re-run should pass. `518-huge_entity` is the same family.

### `safir_control.0.returncode` + `syslog_output` (correlated; overload-triggered)

Seen together on multinode `vs2022` (run 32461278855 = CI #132, 2026-08-21). The
`safir_control.0.returncode` junit says only `Process exited with return code 1,
expected 0`; because the dose runner treats a bad `safir_control` *process*
returncode as infra, this one **reddens the job**, not just the Check.

**This is NOT the Windows Defender false positive** documented in AGENTS.md. That
one is DebugOnly (`WinError 225`, the image never starts) and, per AGENTS.md, never
hits GHA (which builds `Full` only). Here the process started, ran, then exited
nonzero.

**Root cause on #132, from the `syslog_output` capture** (this is why you always
read the syslog body — see below). The syslog tells the whole story in order:
1. `Boost.Asio latency for 'SpRawHandler' is at 5583 ms … 7902 ms … your system is
   overloaded` — the GHA runner stalled for **5–8 seconds**.
2. For ~4 minutes, `Excessive retransmits (67) to node Server_0(999999) … excluding
   it!` from the other nodes — under the stall, UDP acks to `Server_0` were lost,
   so the reliability layer excluded it.
3. `CTRL: Caught 'std::exception' … 'Dead node was already defined as alive in last
   state!'. CTRL: Exiting due to error!` — node 0's `safir_control` threw out of
   `io_context.run()` and exited 1. **That is the `safir_control.0.returncode`**,
   and all of the above landing in syslog is what failed `syslog_output`.

The exception is a `std::logic_error` at
`src/distribution/system_picture.ss/src/Coordinator.h:781` — a "sanity check" that
fires when a `SystemStateMessage` lists the same node id as both alive and dead. It
sits next to the node **resurrection** logic (~L745-758). So the chain is: overload
→ mass exclusion of a node → an exclude/resurrect sequence yields a self-
contradictory last-state → the sanity check **aborts the whole node** instead of
reconciling it.

**Classification:** the *trigger* is runner overload (environmental, not our
workflow) → deferred. But this is more than a lost packet: it exposes a **latent
robustness gap** — `Coordinator` treats an inconsistent last-state as fatal under
heavy exclude/resurrect churn, so one bad state message kills the node. That part
is a real product bug and is tracked as **#613**, which also records why the fix is
not decided yet: we have the exception but not the state that caused it.

The sanity checks now call `LogStateInconsistency()` before throwing, which puts a
compact one-line dump of every `node_info` entry (index, id, name, alive/dead) plus
`m_resurrectingNodes` into the system log, and the full state at `lllog(1)`. **If
you see this failure again, grab that line** — it is the missing evidence #613 is
waiting for.

### `syslog_output` is a canary — read its body

`syslog_output` fails whenever *anything* unexpected reaches syslog during a run,
so it is not one bug but a detector. Its junit `<failure>` body quotes the actual
syslog lines, which frequently explain a *co-occurring* failure in the same run
(as with `safir_control.0.returncode` on #132 above). Always read the body before
dismissing it; the content differs run to run.

## Fixed / dormant

Kept here because these were long-standing, well-known reds. For a **fixed** one, a
new occurrence is a **regression**, not the old flake, and the notes say where to
look. A **dormant** one was never fixed — it simply stopped appearing, so treat a
recurrence as the same old bug waking up.

### `Communication_ResetTest` — FIXED 2026-06-30 (was Windows-only)

`src/distribution/communication.ss/tests/reset_test/main.cpp`. A long-standing
intermittent, exclusively on Windows (vs2019/vs2022/vs2026); it never failed once
across 69 clean Linux executions. Fixed in **37fd610245f** (issue **#434**, closed
2026-06-30). Last observed failure: run 28340817399, 2026-06-29 — the day before
the fix. No occurrence in any run since, so **do not treat a `Communication_ResetTest`
red as a known flake**; investigate it as new.

There were two independent bugs, matching the two failure modes seen in the logs:

**Mode A — data never arrives** (`recvCount(3)==0`, `BOOST_CHECK` fails).
Real bug in the Reset path, in `DeliveryHandler::Start()`: it zeroed
`m_numberOfUndeliveredMessages`. That counter is incremented on `m_receiveStrand`
and decremented from a lambda on `m_deliverStrand`, and the two strands are not
synchronised across `Stop`/`Start`. Zeroing on `Start` races with in-flight
decrements posted before `Stop`, which then underflow the unsigned counter to
`UINT_MAX` and **permanently freeze the reader** — hence a full 60 s of zero
received data. The fix simply stops resetting the counter (with a comment
explaining why). This is production code, but `Reset()` is not used by any real
code path, so no shipped behaviour was ever affected.

**Mode B — teardown deadlock** (ctest 180 s TIMEOUT, not an assertion). A pure
test-harness bug: `reset_test/Receiver.h` and `Sender.h` called
`m_com.Stop(); m_work.reset(); m_io.restart(); m_threads.join_all();`.
`io_context::restart()` is only valid once the context has fully stopped, and here
it ran while the workers were still inside `run()`. That races benignly on Linux
but on Windows/IOCP could wedge a worker so `run()` never returned, so
`join_all()` hung and the next node's `Stop()` was never reached. Fixed by
dropping `restart()` — these objects are not reused.

Log fingerprints, if you ever need them again: Mode A shows either no
`OnNewNode Sender_3` at all (discovery handshake lost) or `OnNewNode` followed by
`recvCount(3)` stuck at 0; Mode B shows unequal Stop counts (e.g. `Stop Sender_2`×2
vs `Stop Sender_3`×1), the wedged node going silent, and a surviving node
retransmitting its unacked window forever (`Retransmit MultiReceiver …` /
`Cant remove seq …`) — that storm is a symptom, not the cause.

`lllog` level 9 is enabled for this test
(`src/tests/test_support/test_config/logging.ini`); logs land per-process in
`<build>/.../tests/reset_test/test_output/Communication_ResetTest/<procname>-<pid>.txt`,
one new file per run. Classify a captured run by the internal timestamp span
(first vs last `[HH:MM:SS]`), **not** by file size (passing runs range 13–46 MB).

**One unrelated defect was found during that investigation** and is now tracked as
**#614**: `DataReceiver`'s `AsyncReceive` completion handler returns without
re-arming on any error other than `operation_aborted`, which would permanently kill
that socket's read loop. It was a candidate explanation for the Mode A signature,
but was never confirmed and is not what actually caused it. No occurrence has ever
been observed; #614 says a repro comes first.

> The code-level line references above were accurate as of the 2026-06/07
> investigation; verify against current source before acting on them.

### `361-inject_update_and_delete_for_existing_entity` — dormant, NOT fixed

`src/tests/dose_test.ss/testcases/361-inject_update_and_delete_for_existing_entity.xml`.
Reported as issue **#397** (2022-08-30, closed 2026-08-25 under the tracking policy
above). Standalone and multinode dose, seen on Jenkins on `cpp-dotnet-java` and
`dotnet-java-cpp`.

**Signature:** a race between the injection and the read-back. Either the entity is
not there yet — `Caught Exception in ExecuteAction: Safir.Dob.NotFoundException`
where the expected output has a full `Read entity` block — or it is there but stale,
showing `First inject` where `Second inject` was expected. Both partners diff
identically.

**Status: not fixed.** 94c1b94da (2022-08-30) added sleeps after the injections to
absorb slow-VM timing; the test failed again six days later with the same race, so
that fix is known **not** to have held. It has simply not been observed since —
zero occurrences across the last 40 `ci.yml` runs (2026-06-28 → 2026-08-25), and
nothing on GHA at all. Likely the faster runners hide it rather than the race being
gone. If it reappears, do not reach for more sleeps: the real question is what
guarantees the injected update has reached partner 0 before the read-back runs.
