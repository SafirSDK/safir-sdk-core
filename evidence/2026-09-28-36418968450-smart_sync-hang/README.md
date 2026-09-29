# run_light_nodes_smart_sync_tests hang, 2026-09-28

| | |
|---|---|
| Run | [36418968450](https://github.com/SafirSDK/safir-sdk-core/actions/runs/36418968450) |
| Job | `debug-slow-tests-ubuntu-noble-amd64` (108931711843) |
| Commit | `0d746768c` on `private/startup-synchronizer-review` |
| Platform | ubuntu-24.04, Debug build |
| Symptom | driver hung in its fifth case, killed at its 1200 s budget; job red, no junit failure |

Analysis is in [`../../ANALYSIS.md`](../../ANALYSIS.md). This directory is the
evidence, kept because GitHub deletes logs and artifacts after 90 days — the
artifacts for this run expire on 2026-12-27, after which the link above still
resolves but shows nothing useful.

## What is here

- `driver-log-excerpt.txt` — the `run_light_nodes_smart_sync_tests` section of the
  job log, plus the umbrella's closing summary. Timestamps stripped to the message.
  This is the only place the failure is visible at all, so read it first.
- `junit-previous-green.xml` — the same driver on the previous run of this job
  (36345391363, `private/tools-found-bugs`, 2026-09-27): 6 cases, 0 failures,
  408 s. The baseline that makes this occurrence notable.
- `junit-same-commit-rerun-green.xml` — the same job re-run on the *same commit*
  afterwards: 6 cases, 0 failures, 402 s. This is what establishes the failure is
  not deterministic.

## What is deliberately absent

**There is no junit for the failing run.** The driver writes each case's results
and its node logs when it tears the environment down, and this failure killed it
*during* teardown — so the junit was never written and the node logs never landed.
Eleven of the twelve drivers reported; this one did not, and that absence is how
the failing driver was identified in the first place.

The same mechanism means there are no `safir_control`, `dose_main` or `safir_web`
logs for the failing case anywhere, in the artifacts or here. If this recurs and
you want those, reproduce it locally — `ANALYSIS.md` records the harness and the
one host prerequisite (a multicast loopback route).
