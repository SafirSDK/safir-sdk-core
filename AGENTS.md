# AGENTS.md

Guidance for AI agents that land on the `test-status` branch of
[safir-sdk-core](https://github.com/SafirSDK/safir-sdk-core).

## Read this first: there is no source code here

This is an **orphan branch**. It was created with `git switch --orphan`, so it
shares no history with `develop` and has never contained the source tree. If you
are in a working tree of this branch, the three or four files you can see are all
there is — the code was not deleted, it was never here.

Do not go looking for `CMakeLists.txt`, `src/`, or the real `AGENTS.md`. Those are
on `develop`:

    git show origin/develop:AGENTS.md
    git show origin/develop:src/...

Everything about building, testing, code style and the architecture lives there.
Nothing in this file applies to the code.

## What this branch is for

It is the record of **which CI tests fail intermittently, why, and how to tell a
flake from a real regression.**

| file | what it is |
|---|---|
| `LEDGER.md` | append-only log, one line per occurrence. Never rewritten. |
| `ANALYSIS.md` | policy, how to read a red run, per-flake theories, post-mortems |
| `evidence/` | kept material from occurrences that were investigated |
| `README.md` | the full workflow, and why this branch exists at all |

`README.md` has the five-step decision path for "CI went red, now what". Read it
before you write anything. The two rules you are most likely to get wrong:

- **One line per test per run.** The same test failing on three platforms in one
  run is *one* line, with the spread in the note.
- **A flaky test never becomes a GitHub issue.** That is a standing decision, and
  the reasoning is in `ANALYSIS.md`. An issue is for a decision to investigate, or
  for a real product bug found while investigating.

## Reading it without checking it out

You almost never need a working tree to read this. From a normal clone of the
repository, on whatever branch you are already on:

    git fetch origin test-status
    git show origin/test-status:LEDGER.md
    git show origin/test-status:ANALYSIS.md

Or through the API, quoting the URL because an unquoted `?` is a glob in zsh:

    gh api "repos/SafirSDK/safir-sdk-core/contents/LEDGER.md?ref=test-status" \
      --jq .content | base64 -d

Counting is a query, not something anyone maintains:

    grep -c 215-huge_service LEDGER.md
    grep 2007-lightnode LEDGER.md | tail -1

## Appending

**Never check this branch out over a source working tree.** Use a throwaway
worktree, append, push, and remove it:

    git fetch origin test-status
    git worktree add /tmp/test-status test-status
    cd /tmp/test-status
    # append to LEDGER.md; add to ANALYSIS.md if the diagnosis changed
    git commit -am "<test>, run <id>"
    git push
    cd - && git worktree remove /tmp/test-status

Notes that matter for an agent specifically:

- **Pushing here starts no CI.** `ci.yml` triggers on `master`, `develop`,
  `feature/**`, `private/**` and version tags; this branch matches none of them.
  Do not wait for a run that will never appear, and do not treat the absence of a
  green check as a problem.
- **Do not merge this branch into anything, and do not merge anything into it.**
  There is no merge base, which is a safety feature — a careless merge cannot wipe
  the source tree. If a tool reports "entirely different commit histories", that
  is correct, not an error to fix.
- **Do not force-push.** This branch is the only copy of this information, has no
  CI and no review, and is not protected.
- **Mind your directories.** If you are working in a worktree of this branch, your
  other working tree still has the source checked out on another branch. Check
  where you are before running anything.
- Copyright headers are not used on this branch; `develop`'s rule about adding the
  current year applies to source files, and there are none here.

## When you have finished an investigation

Write down what you **ruled out**, not just what you suspect. Six weeks later that
is the more valuable half, and it is the half that is never reconstructible: the
run logs and artifacts are deleted by GitHub after 90 days, which is why
`evidence/` exists and why `README.md` tells you to pull material before it
expires rather than after.
