# Occurrence ledger

Append-only. **Newest last.** One line per observed occurrence, so that counting is
something you do with `grep -c` rather than something anyone has to maintain by
hand. Do not rewrite or tidy old lines; if a diagnosis changes, that goes in
[`ANALYSIS.md`](ANALYSIS.md).

Format, space-aligned for reading but only the field order matters:

    DATE        RUN          SHA        WHAT                            WHERE            NOTE

- `DATE` — UTC date of the run.
- `RUN` — the `ci.yml` run id.
- `SHA` — the commit that was tested. **Record this.** The run id stops being
  useful once GitHub expires the run's logs and artifacts at 90 days, and then the
  sha is the only thing left that says what code produced the failure. Note that a
  sha on a `private/**` branch can itself become unreachable if that branch is
  rebased and deleted, so for anything worth keeping, keep evidence too (below).
- `WHAT` — the test case, or the driver name for a job-level hang.
- `WHERE` — the suite and platform it happened on.
- `NOTE` — anything that distinguishes this occurrence from the others. Keep it to
  one line; long-form goes in `ANALYSIS.md`. If there is an evidence bundle, say
  `evidence/<dir>`.

**One line per test per run.** If the same test fails on three platforms in one
run, that is one line, with the spread recorded in the `NOTE` ("3 of 8
multicomputer legs"). A run is the unit because that is how flakiness is
experienced — one red run is one event to triage — and because the old counts were
per run too, so the history above and the lines below mean the same thing. Two
different tests failing in one run are two lines.

Useful queries:

    grep -c 215-huge_service LEDGER.md              # how many times
    grep 215-huge_service LEDGER.md | tail -1       # most recent
    tail -20 LEDGER.md                              # what has been happening lately

## Before this ledger existed

It starts on 2026-09-29. Everything before that was recorded only as aggregate
counts in the old in-repo `TEST_STATUS.md`, against a scan of the 100 most recent
runs (2026-06-16 → 2026-08-26) of which 89 produced a "Test results" Check and were
therefore countable. Those counts cannot be expanded into per-occurrence lines
after the fact, so they are preserved as history here rather than invented:

| Test | Suite | Occurrences / 89 countable runs | Last seen in that window |
|---|---|---|---|
| `215-huge_service` | multicomputer dose (overlay) | 23 | 2026-08-26 (32999608623) |
| `518-huge_entity` | multicomputer dose (overlay) | 3 | 2026-08-20 (32348188649) |
| `155-pending_service_registration_same_node` | dose | 3 | 2026-08-20 (32348188649) |
| `2007-lightnode_limited_entity_on_normal_node` | multicomputer dose | 2 | 2026-08-21 (32461278855) |
| `353-pending_entity_handler_registration_between_nodes` | multicomputer dose | 1 | 2026-08-18 (32143455257) |
| `HeartbeatSenderTest` | slow suite (`run_communication_tests`), Windows | 1 | 2026-08-18 (32143455257) |

32 of those 89 runs — a bit over a third — had at least one failing test case, and
`215-huge_service` alone accounted for 23 of them, more than everything else
combined. Counts were per *run*, not per test execution; one run covers roughly 34
dose executions across the matrix.

Two job-level hangs from the same period have no count because they never appear in
the junit at all: `run_restart_nodes_tests` (last seen 2026-08-23, run
32637686999) and `run_light_nodes_smart_sync_tests` (2026-09-28, run 36418968450).

## Occurrences

    DATE        RUN          SHA        WHAT                                          WHERE                                    NOTE
    2026-08-18  32143455257  a077872d4  353-pending_entity_handler_registration_...    multicomputer dose                       backfilled from the old aggregate table
    2026-08-18  32143455257  a077872d4  HeartbeatSenderTest                            slow suite, Windows                      backfilled from the old aggregate table
    2026-08-20  32348188649  0f070ce71  518-huge_entity                                multicomputer dose (overlay)             backfilled from the old aggregate table
    2026-08-20  32348188649  0f070ce71  155-pending_service_registration_same_node     dose                                     backfilled from the old aggregate table
    2026-08-21  32461278855  a025bc421  2007-lightnode_limited_entity_on_normal_node   multicomputer dose                       backfilled from the old aggregate table
    2026-08-23  32637686999  1ffeb0dd9  run_restart_nodes_tests (hang)                 slow suite                               job-level TIMEOUT, no junit entry
    2026-08-26  32999608623  5283b3338  215-huge_service                               multicomputer dose (overlay)             backfilled; fixed 2026-08-27
    2026-09-28  36418968450  0d746768c  run_light_nodes_smart_sync_tests (hang)        Debug slow suite, ubuntu-noble-amd64     job-level TIMEOUT, no junit; attribution open; evidence/2026-09-28-36418968450-smart_sync-hang
    2026-09-28  36487125418  fffc43880  2007-lightnode_limited_entity_on_normal_node   multicomputer dose, ubuntu-noble-arm64   1 of 36 dose runs; job green, "Test results" Check red
    2026-10-01  36916554564  a62ab20dc  multicomputer-tests (hang)                     multicomputer dose (overlay), ubuntu-noble-amd64   job-level TIMEOUT (55 min), no junit; StartupSynchronizer ruled out on all 4 nodes; evidence/2026-10-01-36916554564-multicomputer-sequencer-hang

The shas for the backfilled rows were recovered from the run records on
2026-09-29, while those still existed. Four of them are on
`private/move-slow-unittests`, so if that branch is ever deleted and garbage
collected they will stop resolving; that is exactly the decay this column exists
to slow down, not something it can prevent.
