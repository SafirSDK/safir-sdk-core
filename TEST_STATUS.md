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

**Seen** and **Last seen** come from a scan of the 100 most recent `ci.yml` runs
(2026-06-16 → 2026-08-26) plus every run since, of which **87 produced a "Test
results" Check** and are therefore countable; the rest were cancelled or died
before the tests. 32 of those 87 runs — a bit over a third — had at least one
failing test case. Counts are per
*run*, not per test execution (one run covers ~34 dose executions across the
matrix). Update both columns when you see a fresh occurrence; an entry whose
last-seen keeps receding is a candidate for the dormant list.

`215-huge_service` dominated this list — 23 of 87, more than everything else in it
combined, and its rate was rising (11 of 58 runs in June, 12 of 27 in August). It
was **fixed on 2026-08-27**, so those counts are the historical record rather than
a prediction. Until several full-matrix runs have gone by without it, treat its
absence as unconfirmed rather than proven.

`Communication_ResetTest` used to head this list; it was **fixed on 2026-06-30**
and is no longer flaky — see "Fixed / dormant" below before you re-diagnose it.

| Test | Where | Platform | Seen | Last seen | Character |
|---|---|---|---|---|---|
| `215-huge_service` | multicomputer dose (overlay) | any | **23/87** | 2026-08-26 (32999608623) | **Believed fixed 2026-08-27** — the sleep it raced is gone; see below |
| `518-huge_entity` | multicomputer dose (overlay) | any | 3/87 | 2026-08-20 (32348188649) | Same family as 215, **fixed the same way 2026-08-27** |
| `155-pending_service_registration_same_node` | dose | any | 3/87 | 2026-08-20 (32348188649) | "Pending registration" family |
| `2007-lightnode_limited_entity_on_normal_node` | multicomputer dose | any | 2/87 | 2026-08-21 (32461278855) | Light-node detach/reattach |
| `353-pending_entity_handler_registration_between_nodes` | multicomputer dose | any | 1/87 | 2026-08-18 (32143455257) | "Pending registration" family |
| `HeartbeatSenderTest` | slow suite (`run_communication_tests`) | Windows | 1/87 | 2026-08-18 (32143455257) | Communication flake, newly visible via slow-suite junit |
| `run_restart_nodes_tests` (hang) | slow suite | any | n/a | 2026-08-23 (32637686999) | Hangs at startup → TIMEOUT → **fails the job**; job-level, not in the junit counts |

**Two names that used to appear in this table are deliberately absent:
`syslog_output` and `safir_control.0.returncode`.** Neither is a test. They are the
two channels through which a *product* error gets reported at all — "nothing was
logged" and "the node process exited 0" — so a red there says an error occurred,
not that a flaky test flaked. Counting them alongside real tests produces a number
that describes nothing (the ten `syslog_output` reds were at least three unrelated
defects) and, worse, implies the standard response of re-running. Each occurrence
has to be classified on the *content* of what was reported. See "Errors reported
through `syslog_output` and `safir_control.0.returncode`" below.

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
- **Third-party package fetches in "Set up build environment."** A whole matrix
  leg dies before it builds anything, taking every job that depends on it with it.
  The failure is always in step 4 and always someone else's download:
  - **CI #148** (run 32874768890, `vs2022-amd64`): `choco install nsis
    doxygen.install graphviz dia dejavufonts gzip` hung **45 minutes** pulling
    doxygen 1.18.0 from `sourceforge.net`, hit chocolatey's default 2700 s
    execution timeout, and exited 127. Graphviz wobbled in the same step
    ("Attempt to use original download file name failed") but recovered; only
    doxygen actually failed.
  - **CI #145** (run 32872859767, `ubuntu-noble-arm64`): `apt` 404s on
    `openjdk-21` (`21.0.12+8-1~24.04_arm64.deb` no longer on ports.ubuntu.com —
    the archive had moved to a newer point release while the runner image's
    index was stale), exit 100.

  Recognising it: the "Test results" Check stays **green but tiny** — #148 shows
  228 tests / 4 files against a normal ~9193 — because the downstream dose,
  multicomputer and slow jobs are *skipped*, not failed. A small green Check next
  to a red run is the fingerprint. Don't read the shrunken test counts as a
  signal, and don't conclude a flake got fixed because it's absent from a run
  where most of the matrix never executed.

  It recurred the same evening — **CI #149** (run 32897163638) lost *both* legs at
  once: the same apt 404 byte-for-byte on `ubuntu-noble-arm64`, and doxygen from
  sourceforge again on `vs2022-amd64`, this time failing in ~9 minutes with
  `Received an unexpected EOF or 0 bytes from the transport stream` instead of
  timing out. Three occurrences across three consecutive runs retired the "bad
  day" theory.

  **Fixed 2026-08-26** by retrying every package install in
  `.github/actions/setup-build-env` (see `retry.sh` there), not just the one that
  had hurt us before. If a fetch fails now it is retried for about two hours
  before the job gives up, so **a red setup step means a genuine outage, not a
  blip** — read the log rather than re-running. Note the apt case needs
  `update`+`install` retried *together*: the 404 was for a version the stale
  index still advertised, so retrying `install` alone would ask for the same
  missing filename every time.

  What was *not* done: the packages are still fetched from third-party mirrors on
  every run. Caching or self-hosting them is the only thing that would make this
  independent of someone else's uptime; retrying just rides the outage out.
- **A job timeout reports as `cancelled`, not `failure`.** GitHub kills a job that
  exceeds `timeout-minutes` with `##[error]The operation was canceled.`, so the
  *run* ends up "cancelled" and looks like somebody pressed the button. Check the
  job duration against its `timeout-minutes` before assuming a human or a newer
  push cancelled it.

  Seen on **CI #150** (run 32950883682): two of the three
  `multinode-ubuntu-noble-arm64` jobs were killed at exactly 30m16s, both at
  testcase **9999** — the last one in the suite, so they had done all the work and
  reported none of it. Everything else in the run was green.

  **Cause: no margin, on arm64 only.** Observed dose-tests durations (CI #150,
  worst case per platform):

  | Platform | standalone | multinode |
  |---|---|---|
  | ubuntu-noble-**arm64** | **27 min** | **28 min** |
  | ubuntu-noble-amd64 | 17 | 19 |
  | debian-trixie-amd64 | 18 | 18 |
  | vs2022 / vs2026 | 18 | 18-19 |

  arm64 is ~1.5x slower than everything else and had been running 27 min against a
  30 min limit — 9% of headroom — in every recent run (#136, #137, #144). Raised
  to 60 min on 2026-08-26 (~2.2x observed), matching how the slow-suite driver
  timeout was picked. **If an arm64 dose job times out again, do not just raise
  the number**: 60 is now over twice the runtime, so a job hitting it is hanging,
  which is exactly what the timeout is for. For reference the slow-tests jobs run
  72-73 min against their 150 (2.1x), so they have the same kind of margin.
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
overlay. First seen **2026-06-25** (run 28167281373, on
`private/github-actions-multicomputer-tests-squashed`) — i.e. it has been failing
since the multicomputer suite first ran on GHA, not since August as this file
previously claimed. A typical occurrence: aggregation reports "1 out of 34 runs
failed", the workflow still concludes `success`, and only the "Test results" Check
goes red.

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

**What the payload actually is:** the testcase XML contains no huge data. The
Sequencer fills any *null* `BinaryMember` with **10 MB** before sending
(`Sequencer.cpp:64-80`), which is exactly why the "huge" tests omit the member and
their "complex" siblings (`214`, `517`) include an empty `<BinaryMember/>` and stay
small.

**The binding deadline is the testcase `Sleep`, not the request timeout** (found
2026-08-27, run 32999608623). `DoseTest.ComplexGlobalService` declares a **120 s**
request timeout (`dose_test_dou.ss/data/DoseTest.ComplexGlobalService-Safir.Dob.RequestTimeoutProperty.dom`),
raised from the 7 s default for exactly this reason — but it can never be reached.
The action after the sleep is `Reset`, and the partner log shows `Performing Reset`
→ `Calling Close` → `Calling Open`: the connection is torn down when the sleep
expires, abandoning anything in flight. So the real budget for a 10 MB round trip
was the **20 s** sleep, while its siblings `009` and `518` had 30 s. Raising the
`.dom` timeout would have done nothing.

**Evidence it is slow rather than lost.** Testcase duration on run 32999608623,
measured from `Running testcase:` timestamps in the job logs:

| platform | 215 → 300 |
|---|---|
| vs2022 | 22.7 s |
| ubuntu-noble-amd64 | 22.0 s |
| ubuntu-noble-arm64 | 22.8 s |
| **vs2026 (the failure)** | **28.1 s** |

A passing run is 20 s sleep + ~2 s overhead; the failing one spent ~8 s of non-sleep
time, i.e. the transfer was labouring and the window closed on it.

**Why "the message was lost" was the obvious reading.** `Reset` deletes the
consumers and deregisters the handler, so a late request has nowhere to land and is
never logged — the teardown destroys the evidence. Partner 0's log genuinely shows
no `OnServiceRequest`, but that is equally consistent with "arrived at 21 s". Do not
read the absence as proof of loss. Note also that keeping state alive to catch late
callbacks is **not** an option: the fresh state `Reset` provides is part of the test
strategy, and weakening it would trade this flake for order-dependent ones.

**Fixed 2026-08-27 by removing the deadline** rather than enlarging it. The
testcase no longer sleeps at all: it waits for the callbacks it actually needs
(`OnServiceRequest` on partner 0, `OnResponse` on partner 2) using the
`WaitForCallback` action added the same day. The round trip now has 60 s to
complete instead of 20, and normally costs a couple of seconds rather than the
whole sleep. `518-huge_entity`, `517-complex_entity`, `009-huge_message` and
`369-pending_and_override_with_instances` were converted the same way.

**This flake cannot recur in the same form** — there is no fixed window left for a
slow transfer to overrun. What can still happen is the 60 s backstop expiring, and
it looks quite different: the partner's `.output.txt` says `WaitForCallback: done
waiting for OnServiceRequest occurrence 1 (TIMED OUT)`, while the junit diff is the
same missing-output one as before. **If you see that, don't go hunting for a lost
message** — it means a 10 MB round trip took over a minute. The answer is either to
raise `WaitForCallbackTimeoutSeconds` (one constant, in the three partner
Executors) or to find out what the overlay is doing.

**Still unexplained:** *why* the transfer was ~4x slower on the run that failed.
Waiting for the callback tolerates that; it does not diagnose it. If the huge tests
start timing out, that is the thing to chase.

## Errors reported through `syslog_output` and `safir_control.0.returncode`

**These two are not tests and not flakes.** `syslog_output` asserts that nothing
unexpected reached syslog during a run; `safir_control.0.returncode` asserts that
the node process exited 0. They are how a product error in *any* subsystem becomes
visible to CI at all, which means a red here can be anything from an overloaded
runner to a genuine bug, and the failures grouped under one of these names have
nothing to do with each other beyond the channel they arrived on.

So there is no "known flake" answer for them. **Classify every occurrence by the
content of the `<failure>` body** — it quotes the actual log lines — and treat it
as whatever that content says it is. The two also correlate: a fatal error usually
lands in syslog *and* kills the process, so one incident reddens both (a
`safir_control` process exit is treated as infra by the dose runner and fails the
whole job, while `syslog_output` is a carried assertion).

### What has actually been reported this way

All 10 `syslog_output` occurrences in the scan window were pulled from the run
artifacts and read. Two are the same commit built twice (`6744f0de5a`), leaving
nine distinct failures in three unrelated groups:

| Reported error | Count | Severity | Source |
|---|---|---|---|
| `Boost.Asio latency for 'SpRawHandler' is at N ms … your system is overloaded` | 6 | Warning | `AsioLatencyMonitor.h` |
| `One or more items seem to be stuck in WaitingStates!` | 2 | Warning | `dose_main.ss/src/WaitingStates.cpp:255` |
| `DOSE_MAIN: Got a request that was neither sent to or from this node!` | 1 | Error | `dose_main.ss/src/RequestHandler.cpp:571` |

**Runner overload (6).** Observed 1391-7902 ms, on vs2026 standalone and multinode
and on ubuntu-noble multinode. Environmental, and on its own the one case here that
genuinely warrants a re-run. But it is also the first link in the #132 chain, where
a 5-8 s stall cascaded into four minutes of `Excessive retransmits … excluding it!`
and then the `Coordinator` throw that killed the node (#613, and the single
`safir_control.0.returncode` occurrence in the window). Note how big the stall was
and whether anything follows it.

**Stuck in WaitingStates (2).** From `SanityCheck()`, when an item is still queued
on two consecutive checks. Both occurrences were the same job — multinode
`ubuntu-noble-arm64`, `java-cpp-dotnet-java-cpp` — and both quote `TracerStatus`
and `MirroredNodeInfo` states. The message says it can be ignored if the system was
artificially stopped, which is plausible at test teardown, but nobody has confirmed
that is what happened here. Unclassified.

**Misrouted request (1).** Seen once, multinode `ubuntu-noble-amd64`,
`dotnet-java-cpp-dotnet-java`. A request reached node 0's `dose_main` with neither
endpoint local — sender connection on node **66666**, receiver
`Server_1.888888;0;safir_control_status;main#8` — so it was neither sent to nor
from the node that logged it. `senderFound=1`, i.e. node 0 did know the sender
connection.

All three ids belong to this suite: `run_dose_tests.py.in:46-49` assigns
`server-0: 999999`, `server-1: 888888`, `client-0: 77777`, `client-1: 66666`. So
this is not traffic from outside the system — it is a request between two of the
suite's own nodes arriving at a third.

**Nobody has diagnosed this.** It has been seen once, nothing has been
investigated, and the notes above are just what the log line and the surrounding
code say. Everything below is guesswork from reading that one line.

Two things it *might* be: crosstalk between testcases, or simply a delayed message.
The node ids are fixed for a whole dose run and reused by every testcase in it, so
a straggler from an earlier testcase would arrive during a later one still carrying
ids that look valid. That is a guess, not a finding — it has not been checked
against the run.

It may well be harmless: `RequestHandler::DistributeRequest` logs and then returns
`true` — "Always OK, request not for us" — so the request is dropped. If it is a
leftover addressed to a node that is no longer part of the running system, dropping
it is the right thing to do and the log line is the only consequence.

If it recurs, capturing which testcase was running and what ran immediately before
would be a reasonable place to start. Deliberately not filed as an issue: one
occurrence and no reproduction.

### The #132 incident in full (overload → exclusion → node death)

Seen together on multinode `vs2022` (run 32461278855 = CI #132, 2026-08-21). The
`safir_control.0.returncode` junit says only `Process exited with return code 1,
expected 0`; because the dose runner treats a bad `safir_control` *process*
returncode as infra, this one **reddens the job**, not just the Check.

**This is NOT the Windows Defender false positive** documented in AGENTS.md. That
one is DebugOnly (`WinError 225`, the image never starts) and, per AGENTS.md, never
hits GHA (which builds `Full` only). Here the process started, ran, then exited
nonzero.

**Root cause on #132, from the `syslog_output` capture** (this is why you always
read the syslog body). The syslog tells the whole story in order:
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
workflow) → nothing to fix there. But the incident is more than a lost packet: it
exposes a **latent robustness gap** — `Coordinator` treats an inconsistent last-state as fatal under
heavy exclude/resurrect churn, so one bad state message kills the node. That part
is a real product bug and is tracked as **#613**, which also records why the fix is
not decided yet: we have the exception but not the state that caused it.

The sanity checks now call `LogStateInconsistency()` before throwing, which puts a
compact one-line dump of every `node_info` entry (index, id, name, alive/dead) plus
`m_resurrectingNodes` into the system log, and the full state at `lllog(1)`. **If
you see this failure again, grab that line** — it is the missing evidence #613 is
waiting for.

## Waiting instead of sleeping

Testcases used to wait only by sleeping, and those sleeps were unconditional: 659 s
of them per dose run, paid whether or not anything was slow. Worse, a sleep was
also the *deadline* for whatever it waited for, because the `Reset` that follows
closes the connection and abandons anything still in flight. That is what made
`215-huge_service` the most frequent flake in CI.

The `WaitForCallback` action (added 2026-08-27) lets a testcase wait for the event
instead. The partner withholds the three-byte `"ok"` the sequencer is already
blocking on, so no sequencer change was needed, and acknowledges when the callback
arrives or after a 60 s backstop. Five testcases were converted — `215`, `518`,
`517`, `009` and `369` — removing **260 s of sleeping per dose run** (659 → 399).

Two things about it are worth knowing before touching it:

- **Occurrences are numbered, not consumed.** `WaitForCallbackOccurrence` says
  *which* occurrence to wait for, counted per testcase. A testcase with several
  phases sees the same callback once per phase — `518` gets three `OnResponse` —
  and without the number every wait after the first is satisfied by the first
  phase's callback, waits for nothing, and still passes. The failure is invisible
  in the test output, so this is not an optimisation to simplify away.
- **The counts live on the Executor, not the Consumer**, and are cleared only by
  `Reset`. `369` closes and reopens a partner three times mid-testcase; per-Consumer
  counts would be wiped each time and every occurrence number in it would be wrong.

What is left in the 399 s mostly cannot be converted: sleeps that exist to show
that *nothing further* arrives have no event to wait for, `430`/`431` are testcases
whose whole point is a slow subscriber, and `815` waits on dope writing persistence,
which produces no partner callback.

## Fixed / dormant

Kept here because these were long-standing, well-known reds. For a **fixed** one, a
new occurrence is a **regression**, not the old flake, and the notes say where to
look. A **dormant** one was never fixed — it simply stopped appearing, so treat a
recurrence as the same old bug waking up.

### `Communication_ResetTest` — FIXED 2026-06-30 (was Windows-only)

`src/distribution/communication.ss/tests/reset_test/main.cpp`. A long-standing
intermittent, exclusively on Windows (vs2019/vs2022/vs2026); it never failed once
across 69 clean Linux executions. Fixed in **37fd610245f** (issue **#434**, closed
2026-06-30). It failed in 6 of the 85 countable runs in the scan window, **every
one of them before the fix**: first 2026-06-24 (28093636002), last 28340817399 on
2026-06-29, the day before the fix. Nothing since, so **do not treat a
`Communication_ResetTest` red as a known flake**; investigate it as new. Note that
several of those 6 were reported as ctest *errors* rather than failures — that is
the Mode B teardown timeout below, not an assertion.

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
