# Test status & known flaky tests

Companion to [AGENTS.md](AGENTS.md). This file holds the durable, cross-host
record of **which CI tests fail intermittently, why, and how to tell a flake from
a real regression.** It is kept in-repo on purpose so every developer and agent,
on any host, sees the same triage state. When you diagnose a flake (or fix one),
update this file rather than keeping the notes local.

## Policy: defer flakiness unless it's ours

**A single red CI run is usually a known flake, not your change.** The tests below
fail intermittently for reasons unrelated to the GitHub Actions migration. The
standing decision (2026-08-23) is to **defer all such flakiness to post-merge
issues** and *not* gate the branch on it. The only flakiness worth fixing now is
anything caused by our own workflow/refactoring (a bug in the CI logic, driver
scripts, or test wiring we wrote) — pre-existing test flakiness that merely now
runs on GHA is not a blocker.

When a run goes red, classify each failure before reacting:
- **Pre-existing test flakiness** (everything catalogued below) → re-run; don't
  treat it as a regression.
- **Workflow/refactoring defect** → fix now.
- **Runner-environment quirk** → usually defer, but name it.

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

Frequency below is from the last ~10 branch runs (as of 2026-08-23); it drifts, so
treat it as rough. `215-huge_service` is by far the most common — it turned up in
roughly 5 of the last 8 full-matrix runs, so a lone `215-huge_service` red is the
single most likely thing you'll see.

| Test | Where | Platform | Freq | Character |
|---|---|---|---|---|
| `215-huge_service` | multicomputer dose (overlay) | any | **high** | Huge round-trip occasionally not delivered |
| `Communication_ResetTest` | ctest | **Windows only** | med | Reset/re-discovery UDP flake; deep-dive below |
| `518-huge_entity` | multicomputer dose (overlay) | any | low | Same huge-message family as 215 |
| `syslog_output` | multinode dose | any | low | Intermittent |
| `353-pending_entity_handler_registration_between_nodes` | multicomputer dose | any | low | "Pending registration" family |
| `155-pending_service_registration_same_node` | dose | any | low | "Pending registration" family |
| `2007-lightnode_limited_entity_on_normal_node` | multicomputer dose | any | low | Light-node detach/reattach |
| `HeartbeatSenderTest` | slow suite (`run_communication_tests`) | Windows | low | Communication flake, newly visible via slow-suite junit |
| `safir_control.0.returncode` | multinode dose | **Windows** | low | Genuine `safir_control` exit 1; **fails the job** |

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

### `safir_control.0.returncode` (Windows)

Seen 1/34 on multinode `vs2022` (run 32461278855, 2026-08-21). The junit says only
`Process exited with return code 1, expected 0` — a genuine intermittent
`safir_control` exit. Because the dose runner treats a bad `safir_control`
returncode as infra, this **reddens the job**, not just the Check.

**This is NOT the Windows Defender false positive** documented in AGENTS.md. That
one is DebugOnly (`WinError 225`, the image never starts) and, per AGENTS.md, never
hits GHA because GHA builds `Full` only. Don't conflate the two: here the process
started and ran, then exited nonzero. Root cause not yet isolated.

### `Communication_ResetTest` — deep dive (Windows only)

`src/distribution/communication.ss/tests/reset_test/main.cpp`. A long-standing
intermittent, **exclusively on Windows** (vs2022/vs2026/vs2019); across 69 clean
Linux executions (debian-trixie, ubuntu-noble amd64/arm64) it never once failed.
Reproduced locally on a Windows dev box within ~2–18 runs of
`while ctest . -R Communication_ResetTest --output-on-failure`; never on Linux.

`lllog` level 9 is already enabled for this test
(`src/tests/test_support/test_config/logging.ini`); logs land per-process in
`<build>/.../tests/reset_test/test_output/Communication_ResetTest/<procname>-<pid>.txt`,
one new file per run. Classify a captured run by the internal timestamp span
(first vs last `[HH:MM:SS]`), **not** by file size (passing runs range 13–46 MB).

There are **two independent failure modes**, both Windows-only:

**Mode A — data never arrives** (`recvCount(3)==0`, `BOOST_CHECK` fails). After
`receiver.Reset(seed3, 3)`, node1 delivering any of node3's data hinges on two
fragile single-packet handshakes completing within 60 s over Windows loopback UDP:
1. *Discovery* — node1↔node3 NodeInfo round-trips so node1 fires
   `OnNewNode(Sender_3)` → `IncludeNode(3)`.
2. *WELCOME gating* — in `DeliveryHandler.h`, a `NodeInfo` starts with
   `ackedMultiReceiverChannel.welcome = UINT64_MAX`, and `HandleAckedMessage`
   drops every acked multi-receiver message with `seqNo < ch.welcome`. All of the
   sender's app data (toId=0, guaranteed → acked multi-receiver) lands on this
   channel, so node1 discards 100% of node3's data until it receives node3's single
   Welcome (`DataSender::PostWelcome`, sent once on `IncludeNode`), which sets
   `ch.welcome = seqNo`.

   Sub-signature A1: `OnNewNode Sender_3` never fires (handshake 1 fails).
   Sub-signature A2: it fires, but `recvCount(3)` stays 0 (handshake 2 never
   completes — Welcome never sent or never arrives; reproduced locally as the
   multicast delivery path dropping the Welcome while the unicast case in the same
   process passed).

   **Latent robustness bug, prime suspect for a permanent 60 s of zero:** in
   `src/distribution/communication.ss/src/DataReceiver.h`, the `AsyncReceive`
   completion handler, on any error other than `operation_aborted`, logs "Read
   failed…" and **returns without re-arming `AsyncReceive`** — permanently killing
   the node's receive loop (deaf forever). A transient Windows UDP error
   (`WSAECONNRESET` 10054 from a prior ICMP port-unreachable, `WSAENOBUFS` 10055
   under loopback load) would trip this; `SIO_UDP_CONNRESET` is not set on the recv
   socket anywhere. Not yet proven for a specific captured signature.

**Mode B — teardown deadlock** (ctest 180 s TIMEOUT, not an assertion). Data flows
fine; the hang is purely in teardown. `CommunicationImpl::Stop()` is fully
asynchronous (posts stops to strands and returns). The actual thread-join is in the
*test wrappers* (`reset_test/Receiver.h`, `Sender.h`):
`m_com.Stop(); m_work.reset(); m_io.restart(); m_threads.join_all();`. One node's
wrapper `Stop()` enters but `m_threads.join_all()` never returns (an io worker never
returns from `m_io.run()`), so the next node's `Stop()` is never called. Fingerprint
in the log: unequal Stop counts (e.g. `Stop Sender_2`×2 but `Stop Sender_3`×1), the
wedged node goes silent, and a still-running node retransmits its unacked window to
the dead peer forever (`Retransmit MultiReceiver …` / `Cant remove seq …`) — that
storm is a symptom, not the cause. The wedged node varies across runs.

**Prime suspect:** the wrapper calls `m_io.restart()` **before**
`m_threads.join_all()`, i.e. while the workers are still inside `run()`.
`io_context::restart()` is only valid after the context has fully stopped; calling
it mid-run races benignly on Linux but on Windows/IOCP can wedge a worker so
`run()` never returns. **Likely fix:** `join_all()` first, then `restart()` only if
reusing — or drop `restart()` (these objects aren't reused). This is a
test-harness bug, independent of the Mode-A data-loss bug.

**Do not "fix" this by loosening the 60 s assertions** — the failures are real
Windows flakiness in the Reset/re-discovery/data path. Suggested confirmation
(Windows only): run with COM `lllog` 8–9, capture the signature, then test the two
candidate fixes (re-arm `AsyncReceive` on non-fatal errors + set
`SIO_UDP_CONNRESET`; and reorder `join_all()`/`restart()`).

> The code-level line references in this deep-dive were accurate as of the
> 2026-06/07 investigation; verify against current source before acting on them.
