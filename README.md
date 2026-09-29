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

## Reading it

Without checking anything out, from a normal clone:

    git fetch origin test-status
    git show origin/test-status:LEDGER.md
    git show origin/test-status:ANALYSIS.md

Or through the API, which is convenient for tooling:

    gh api repos/SafirSDK/safir-sdk-core/contents/LEDGER.md?ref=test-status \
      --jq .content | base64 -d

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
