# dose_main waits on persistence that never arrives, standalone, 2026-10-08

| | |
|---|---|
| Run | [37795274382](https://github.com/SafirSDK/safir-sdk-core/actions/runs/37795274382) |
| Job | `standalone-debian-trixie-amd64-amd64-dotnet-java-cpp-dotnet-java` (113399891924) |
| Commit | `ce98a0586` — `develop`, the push that merged the ubsan/tsan-fixes branch on top of the StartupSynchronizer rework (one commit below it) |
| Platform | debian:13 container, single machine, 5 local dose_test partners (dotnet, java, cpp, dotnet, java) |
| Symptom | killed at 59:06 against its 60-minute `timeout-minutes`; normal runtime for this job is ~14 min |

Analysis is in [`../../ANALYSIS.md`](../../ANALYSIS.md) → "dose_main waits on
persistence that never arrives, standalone". This directory is the evidence,
kept because GitHub deletes run logs and artifacts after 90 days — this run's
artifacts expire around 2027-01-06; the comparison run's (36345391363) around
2026-12-26.

## What is here

- `orchestrator-log-excerpt.txt` — the job's own log from the five processes'
  launch through `dose_test_sequencer` reporting "Started", then 59 minutes of
  total silence until the cancellation. Nothing in between — not even the first
  of 10000 testcases reported anything.
- `safir_control-and-dope_main-hung-vs-clean.txt` — the decisive comparison.
  `safir_control.0` (dose_main's own log) is identical for its first four lines
  against the clean run (same job, run 36345391363, 2026-09-27, this branch's
  content before the rebase onto the StartupSynchronizer rework), then never
  reaches `dose_main running...` / `persistence data is ready!`.
  `dope_main.0.output.txt` — the persistence provider's log — is 0 bytes against
  44 in the clean run. `dose_test_cpp.0.output.txt` (and its four siblings) stop
  at `Starting`, before the point that needs a working local dose_main.
- `lock-directory-listing.txt` — `temp/safir-sdk-core/lock/`, archived before the
  kill: `_FIRST`, `_SECOND` and `_CREATED` all present for
  `SAFIR_DOSE_INITIALIZATION`, `SAFIR_DOTS_INITIALIZATION` and `SAFIR_CONTROL_0`.
  The same method the previous entry (2026-10-01) used to rule out
  `StartupSynchronizer` on a different hang.

## What this does and does not establish

**Does:** all 15 client processes (5 each of cpp/dotnet/java) stop at the exact
same relative point - immediately before their own connect to the local
dose_main would succeed - with no exceptions, pointing at one shared cause
rather than many. Six other jobs on the same platform ran clean, start to
finish, entirely inside this job's hang window, which rules out a
runner-pool-wide event at that time. The hang is narrowed to the dose_main ↔ dope_main persistence handoff,
specifically the point where dose_main, having started and logged "waiting for
persistence data!", should receive that data from dope_main and does not.
`StartupSynchronizer` completed successfully on this node (all lock markers
present) — same conclusion as the previous entry, different incident, different
method of confirming it (no overlay here; this is a single-machine standalone
test).

**Does not:** say why dope_main produced nothing. Zero bytes does not distinguish
"dope_main hung before its first `std::cout`" from "dope_main never got
scheduled by the runner at all" — nothing in this pipeline captures a
dope_main-internal log, and no `strace`/process-list snapshot was taken before
the kill. Nor does it establish that the StartupSynchronizer rework caused this:
this branch's own content ran this exact job clean three weeks ago, and the
StartupSynchronizer rework alone ran it clean the same morning as this hang —
only this run, which combines both, has shown it. That is suggestive of an
interaction, not proof of one; it could equally be an ordinary one-off on the
runner, in a process (dope_main) neither change touches.

## What is deliberately absent

No dope_main-internal log, no `strace`, no thread dump — none of that is
currently captured by this pipeline, for any process. If this recurs, that is
the instrumentation worth adding before the next occurrence, not after.
