# lowmem_basic_operations: a Delete at VeryLow was not refused, 2026-10-13

| | |
|---|---|
| Run | [38429866975](https://github.com/SafirSDK/safir-sdk-core/actions/runs/38429866975) |
| Job | `slow-tests-vs2026-amd64` |
| Commit | `8647787fe` on `develop` |
| Platform | vs2026 amd64, RelWithDebInfo, installed slow suite |
| Symptom | `run_lowmem_basic_operations_tests.lowmem_basic_operations` failed after 22 s: `basic_operations` exited 1 with `std::logic_error: Expected failure` |

Analysis is in [`../../ANALYSIS.md`](../../ANALYSIS.md) → "`lowmem_basic_operations`,
an operation that should be refused is not". The run's artifacts expire around
2027-01-11.

## What is here

- `job-log-excerpt.txt` - the whole of the driver's output for this test, from the
  job log: the launch, the test application's output (printed twice by the driver),
  and the verdict. The application's own lines are the evidence; compare them with
  the sequence in `src/tests/low_memory/basic_operations/basic_operations.cpp`.

## What this does and does not establish

**Does:** which step failed. The output stops right after
`Deleted entity instance 5`, which is `DeleteLast(true)`: the test had just
confirmed the level was VeryLow (`CheckLevel` returned; it throws with its own
message otherwise) and expected the Delete to be refused with `LowMemoryException`. It
went through instead.

**Does not:** why. Nothing here shows the memory level at the moment of the Delete.
The junit carries only the driver's traceback (`A process died`), and the
`slow-output-vs2026-amd64` artifact has no directory for this driver, so the job
log is the only record.
