# dose_test_dotnet exits 255 from a finalizer, 2026-10-13

| | |
|---|---|
| Run | [38409610110](https://github.com/SafirSDK/safir-sdk-core/actions/runs/38409610110) |
| Job | `debug-multinode-ubuntu-noble-amd64` (115349719372) |
| Commit | `b6b100b7f` on `develop` |
| Platform | ubuntu-noble amd64, Debug build, multinode dose suite, `java-cpp-dotnet-java-cpp` |
| Symptom | `dose_test_dotnet.2.returncode` failed: the partner exited with 255. All testcases passed. |
| Fixed | `8647787fe` on `develop`, 2026-10-13 |

Analysis is in [`../../ANALYSIS.md`](../../ANALYSIS.md) → "`dose_test_dotnet.N.returncode`,
exit 255 from the `ActionReceiver` finalizer". The run's artifacts expire around
2027-01-11.

## What is here

- `job-log-excerpt.txt` - the returncode check failing and the job's verdict:
  276 cases, 1 failed. The job is red only because the Debug jobs hard-fail on any
  test failure.
- `dose_test_dotnet.2.output-tail.txt` - the end of the partner's own output, from
  the `dose-output-debug-multinode-ubuntu-noble-amd64` artifact. The partner gets
  its stop order, prints `Exiting`, and then dies on an unhandled
  `System.NotSupportedException: Stream does not support writing`, thrown by
  `Console.WriteLine` in `ActionReceiver.Close()` called from
  `ActionReceiver.Finalize()`. The stack trace is the whole diagnosis.

## What this does and does not establish

**Does:** the cause. The finalizer wrote to the console during process exit, after
mono had shut the console streams down. Nothing about the Dob or the test sequence
is involved, and the commit under test (the multicomputer UDP probe) does not touch
this job.

**Does not:** how often it could happen. It needs the finalizer to run late enough
at exit to find the stream closed, which is a race inside mono's shutdown. This is
the first occurrence on record.
