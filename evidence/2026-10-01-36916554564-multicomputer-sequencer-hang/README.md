# multicomputer sequencer hang, cross-node partners never activate, 2026-10-01

| | |
|---|---|
| Run | [36916554564](https://github.com/SafirSDK/safir-sdk-core/actions/runs/36916554564) |
| Jobs | `multicomputer-master-ubuntu-noble-amd64` (110551850553) and `multicomputer-slaves-debian (for ubuntu-noble-amd64 master)` (its paired slaves job) |
| Commit | `a62ab20dc` on `private/startup-synchronizer-review` |
| Platform | ubuntu-24.04 master + 3 debian:13 slave containers (server-1, client-0, client-1), joined by the WireGuard overlay |
| Symptom | both jobs killed at their shared 55 min `timeout-minutes` budget, to the second; normal runtime for this pairing is 18-20 min |

Analysis is in [`../../ANALYSIS.md`](../../ANALYSIS.md) → "multicomputer sequencer
hang, cross-node partners never activate". This directory is the evidence, kept
because GitHub deletes run logs and artifacts after 90 days — the artifacts for
this run expire around 2026-12-30.

## What is here

- `master-log-excerpt.txt` — the master job's log around the hang: the WireGuard
  tunnel verify step passing, the 10-minute setup gap, the sequencer launching and
  activating partners 0 and 1, then 44 minutes of complete silence until the job
  was cancelled. Timestamps kept on the lines that matter, stripped elsewhere.
- `safir_control-all-four-nodes.txt` — `safir_control.0.output.txt` from **all
  four physical nodes** (Server_0/master, Server_1, Client_0, Client_1), pulled
  from the `dose-output-master-ubuntu-noble-amd64` and
  `dose-output-slaves-ubuntu-noble-amd64` artifacts. All four show the same
  incarnation id and `dose_main running...` — the thing that rules out
  `StartupSynchronizer` on every node, not just the master.
- `dose_test_cpp-all-five-partners.txt` — the test application's own output for
  all five partner instances (0-4, one per physical node). Instances 0 and 1
  (local to the master) reached `"Activating"` and opened a listening socket;
  instances 2, 3, 4 (the three remote nodes) all stopped dead at `"cpp:N
  Started"`, right after their *local* DOB connection succeeded and right before
  anything that needs the network. This is what isolates the failure to cross-node
  traffic specifically, as opposed to something generic about startup.
- `master-lock-directory-listing.txt` — the master's own
  `temp/safir-sdk-core/lock/` directory as archived by the job: `_FIRST`,
  `_SECOND` and `_CREATED` all present for `SAFIR_DOSE_INITIALIZATION`,
  `SAFIR_DOTS_INITIALIZATION` and `SAFIR_CONTROL_0`. Direct, physical confirmation
  that `CreateMarker()` ran and succeeded on this node.
- `verification-gap-source.txt` — the two relevant steps from
  `.github/actions/wireguard-overlay/action.yml` and `.github/workflows/ci.yml`
  that between them are the *entire* connectivity check this pipeline does before
  trusting the overlay with real traffic. Read together they show the gap: the
  WireGuard handshake check only covers the transit tunnel between the two runner
  hosts, and the one check anywhere near the extra docker-bridge hop the three
  slave containers sit behind is a plain ICMP ping, one direction only, nowhere
  near the size of a real Dob datagram.

## What this does and does not establish

**Does:** StartupSynchronizer worked, identically, on all four nodes. The hang is
entirely in cross-node DOB entity delivery over the overlay - something this
branch's code never touches. Three independent lines of evidence agree on this,
cited above.

**Does not:** pin down *why* the overlay traffic failed this one time. No packet
capture was taken (nothing in this pipeline captures one), so "a fragment got
dropped" is the leading theory from the setup's own documented MTU/fragmentation
risk, not a confirmed cause. The other three multicomputer pairings in this same
run (vs2026, vs2022, ubuntu-noble-arm64) succeeded, which says this is specific to
this one pairing's attempt rather than a systemic problem that day, but does not
say which specific mechanism failed.

## What is deliberately absent

No `dose_test_sequencer`-internal log beyond its own stdout exists; it has no
separate log file. No packet capture, no `tcpdump`, nothing from inside the
WireGuard tunnel or the docker bridge at the moment of the stall - none of that is
currently collected by this pipeline. If this recurs and the actual mechanism
matters, that is the instrumentation to add before the next occurrence, not after.
