# Occurrence ledger

Append-only. **Newest last.** One line per observed occurrence, so that counting is
something you do with `grep -c` rather than something anyone has to maintain by
hand. Do not rewrite or tidy old lines; if a diagnosis changes, that goes in
[`ANALYSIS.md`](ANALYSIS.md).

Format, space-aligned for reading but only the field order matters:

    DATE        RUN          WHAT                                  WHERE                    NOTE

- `DATE` — UTC date of the run.
- `RUN` — the `ci.yml` run id, so the logs and artifacts are one `gh run view`
  away for as long as GitHub keeps them.
- `WHAT` — the test case, or the driver name for a job-level hang.
- `WHERE` — the suite and platform it happened on.
- `NOTE` — anything that distinguishes this occurrence from the others. Keep it to
  one line; long-form goes in `ANALYSIS.md`.

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

    DATE        RUN          WHAT                                          WHERE                                    NOTE
    2026-08-18  32143455257  353-pending_entity_handler_registration_...    multicomputer dose                       backfilled from the old aggregate table
    2026-08-18  32143455257  HeartbeatSenderTest                            slow suite, Windows                      backfilled from the old aggregate table
    2026-08-20  32348188649  518-huge_entity                                multicomputer dose (overlay)             backfilled from the old aggregate table
    2026-08-20  32348188649  155-pending_service_registration_same_node     dose                                     backfilled from the old aggregate table
    2026-08-21  32461278855  2007-lightnode_limited_entity_on_normal_node   multicomputer dose                       backfilled from the old aggregate table
    2026-08-23  32637686999  run_restart_nodes_tests (hang)                 slow suite                               job-level TIMEOUT, no junit entry
    2026-08-26  32999608623  215-huge_service                               multicomputer dose (overlay)             backfilled; fixed 2026-08-27
    2026-09-28  36418968450  run_light_nodes_smart_sync_tests (hang)        Debug slow suite, ubuntu-noble-amd64     job-level TIMEOUT, no junit entry; attribution open
    2026-09-28  36487125418  2007-lightnode_limited_entity_on_normal_node   multicomputer dose, ubuntu-noble-arm64   1 of 36 dose runs; job green, "Test results" Check red
