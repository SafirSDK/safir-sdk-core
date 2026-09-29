# CI test status — flakiness ledger and analysis

This branch is the record of **which CI tests fail intermittently, why, and how to
tell a flake from a real regression.** It carries no source code at all.

- [`LEDGER.md`](LEDGER.md) — an append-only log. One line per observed
  occurrence. Never rewritten, only added to.
- [`ANALYSIS.md`](ANALYSIS.md) — the policy, how to read a red CI run, the working
  theories for each live flake, and post-mortems of the ones that were fixed.

## Why this is a branch and not a file in the source tree

Because recording an observation should be nearly free, and in the source tree it
was not. The log lived in `TEST_STATUS.md` on `develop`, which meant that noting
"this flaked again" required a commit on whatever branch happened to be checked
out — landing test-triage noise in unrelated feature diffs — and every push to
`master`, `develop`, `feature/**` or `private/**` starts a full build matrix. Once
the cost of writing down an occurrence is a couple of hours of CI, occurrences stop
getting written down.

Nothing here matches those branch patterns, so **pushing to this branch runs no
CI.** That is the point.

The other half of the reason is lifecycle. Source code is versioned: checking out
a tag gives you the code as it was. A flakiness log is not like that — it is a
running account of how CI behaves over time, and a copy of it frozen at an old tag
is worse than useless, because it looks authoritative and is stale. What belongs
next to the code is the *fix* for a flake, which is a commit and its message, and
the decision to chase it, which is a GitHub issue.

## This is an orphan branch — do not merge it

It was created with `git switch --orphan`, so it shares no history with `develop`.
The source tree was never deleted here; it simply was never present. A useful
consequence: because there is no merge base, GitHub will refuse to compare or
merge this branch with any other, and a careless merge cannot wipe the source
tree. If a comparison view tells you these branches have "entirely different
commit histories", that is working as intended.

## Evidence

`evidence/<date>-<run>-<short-name>/` holds what is worth keeping from an
occurrence, because **GitHub deletes run logs and artifacts after 90 days.** That
is measured, not assumed: on 2026-09-29 a run from 2026-06-25 reported zero
artifacts and returned a server error for its logs, while a run from 2026-08-18
still had artifacts, expiring 2026-11-16. So a ledger line older than three months
points at a run that still exists and tells you nothing.

Keep a bundle when an occurrence was actually investigated, or when it is the first
of something. A bundle should have a `README.md` with the run id, job id, commit
sha, platform and symptom, and then only the decisive material:

- the junit for the failing case, if one exists;
- the *relevant excerpt* of the job log, not the whole thing;
- anything else that made the diagnosis, compressed if it is large.

**Keep bundles small — tens of KB.** This is the same repository as the source
code, so every object here is in every clone of safir-sdk-core, forever, for
everyone. A few hundred KB per occurrence is affordable; a 50 MB log tarball is
not, and git will never forget it. If the decisive evidence really is huge, keep
the excerpt and record in the bundle README where the full thing was. Should the
evidence ever outgrow that discipline, the escape hatch is to move `evidence/`
into a repository of its own — the ledger format would not change, only the path.

## Reading it

Without checking anything out, from a normal clone:

    git fetch origin test-status
    git show origin/test-status:LEDGER.md
    git show origin/test-status:ANALYSIS.md

Or through the API, which is convenient for tooling:

    gh api "repos/SafirSDK/safir-sdk-core/contents/LEDGER.md?ref=test-status" \
      --jq .content | base64 -d

Quote the URL — an unquoted `?` is a glob character in zsh and the call fails.

## Appending to it

Do not check this branch out over a source working tree. Use a throwaway worktree:

    git fetch origin test-status
    git worktree add /tmp/test-status origin/test-status
    cd /tmp/test-status && git switch test-status
    # append to LEDGER.md, edit ANALYSIS.md if the diagnosis changed
    git commit -am "2007-lightnode_limited_entity_on_normal_node, run 36487125418"
    git push
    cd - && git worktree remove /tmp/test-status

Appends race the way any shared branch does. If the push is rejected, `git pull
--rebase` and push again — the ledger is append-only, so a rebase of one added
line never conflicts in a way that needs thought.
