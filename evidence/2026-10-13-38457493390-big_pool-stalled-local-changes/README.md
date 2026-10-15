# one_normal_one_light_detach_reattach_light_big_pool, 2026-10-13

| | |
|---|---|
| Run | [38457493390](https://github.com/SafirSDK/safir-sdk-core/actions/runs/38457493390) |
| Job | `debug-slow-tests-ubuntu-noble-amd64` (115524414746) |
| Commit | `f057b8974` on `private/datareceiver-read-robustness` |
| Platform | ubuntu-24.04, Debug build |
| Symptom | `*** ERROR: Incorrect pool on node 1` after the full 610 s wait; 1 of 4 keep-state cases failed |

Analysis is in [`../../ANALYSIS.md`](../../ANALYSIS.md). The artifacts for this run
expire on 2027-01-11.

## What is here

- `driver-log-excerpt.txt` — the `run_light_nodes_keep_state_tests` section of the
  job log, all four cases, with the runner's timestamps stripped (the driver's own
  are kept). The pool dump's 8000 entity lines are left out; what they said is
  summarised below. Line 143 is the one that matters: a second `Node 1 is connected
  to the DOB`, 42 s after the test started sending node 1 its changes.
- `junit-failing.xml` — that driver's junit from the run.

## What the left-out pool dump said

The dump is node 1's client's view of the pool, built from its subscription
callbacks; it is not read from the node. After node 5 detached, the test sent node
1, in this order: the registration changes, updates of `GlobalEntity` 4001-6000 to
`version1`, deletes of 6001-8000, and creates of `GlobalEntity2` 1-2000. The client
had seen:

- the registration changes, all of them;
- updates of 4001-5685, **in order, with no gaps**; 5686-6000 were still `version0`;
- **none** of the 2000 deletes, and **none** of the 2000 creates.

So the view simply stops at one point in the stream. It is not a scattering of
lost changes.

## What is not here

No `safir_control`, `dose_main`, `dope_main` or `safir_web` output, and no syslog:
the light-node drivers threw all of that away when they stopped the nodes. That is
fixed on `private/light-node-test-diagnostics` (7cca4772f), which saves it under
`test_output/` for the existing artifact step, and makes the pool dump start with
how many requests the client sent and how many were answered.
