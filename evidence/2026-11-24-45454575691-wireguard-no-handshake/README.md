# WireGuard overlay: no handshake, 0 B received on both sides

| | |
|---|---|
| Run | 45454575691 (attempts 1 and 2); also 43832118965 |
| Commit | 466882bd7 (`private/system-picture-election-fixes`); 83b685b4e for 43832118965 |
| Jobs | attempt 1: vs2022 and ubuntu-noble-arm64 pairs; attempt 2 (`gh run rerun --failed`): ubuntu-noble-arm64 pair again, job ids 138789734371 (master) and 138789733824 (slaves) |
| Step | "Establish WireGuard overlay", handshake wait, about 3 minutes in |
| Symptom | `WireGuard tunnel did not come up (no handshake with peer)`; `wg show` on both sides says `0 B received`, several KiB sent |

`overlay-both-sides-attempt2.txt` is the overlay action's output from both halves
of the arm64 pair on attempt 2, with the echoed step scripts left out.

## What it shows, and what that rules out

- **Each side configured the other's current key and endpoint.** The master's peer
  is the slaves' public key and STUN-mapped endpoint, and vice versa, and the
  artifacts are the `-attempt2` ones. So this is not the stale-rendezvous failure
  in develop's AGENTS.md ("Known multicomputer overlay failure modes"), which was
  fixed by scoping the artifact name to the run attempt. It also happened on first
  attempts, where there is no earlier artifact to pick up.
- **Not the Windows excluded-port problem.** Both halves here are Linux, the bind
  on 31820 succeeded, and STUN answered.
- **Not the peer wait.** The endpoints were found within seconds.
- **Port translation is not the difference.** The NAT mapped the local port 31820
  to a different public port here (13312 and 59657), but the passing pairs in run
  45418635225 show the same kind of translation (31820 to 24896, 50816, 36992 and
  so on) and still got a handshake on the first try.
- **Not the code under test.** No Safir process had started.

## Theory, not verified

Both sides send handshakes to the peer's STUN-mapped address and nothing ever
arrives. That fits a NAT with endpoint-dependent mapping on one or both runners:
the public port it uses towards the peer is not the one it used towards the STUN
server, so the punched hole is in the wrong place. `stun.py` asks a single server,
so it cannot tell such a NAT from a friendly one. Checking would take a second
STUN query to a different server address from the same local port, and comparing
the two mapped ports.
