# Shared retry wrapper for the package installs in this repo's composite
# actions. Lives one level above them so every action can source it as
# "${GITHUB_ACTION_PATH}/../retry.sh" - GITHUB_ACTION_PATH resolves to the
# directory of the action doing the sourcing, so a shared helper cannot live
# inside any one of them.
#
# Every install here fetches from a third party we do not control - the Ubuntu
# archive, community.chocolatey.org, sourceforge.net, search.maven.org - and any
# of them can fail in a way that has nothing to do with the code being built.
# Observed so far: a 45 minute hang then a chocolatey timeout pulling doxygen
# from sourceforge, an immediate "unexpected EOF from the transport stream" on
# the same URL, apt 404s while the Ubuntu mirror pool is mid-update, and 403
# rate-limiting from search.maven.org that persisted for over an hour.
#
# When one of these hits, a whole matrix leg dies in "Set up build environment"
# before compiling anything, and every job depending on it is skipped. The
# resulting run is red for a reason that no amount of reading the diff will
# explain, so the cost is not the runner minutes - it is somebody's afternoon.
# We are an open source repo and GHA minutes are free, so the trade is
# deliberately lopsided: wait a long time rather than fail and be diagnosed.
#
# Policy (same for every install, on purpose - one number to reason about):
# two hours of wall clock, or 13 attempts, whichever runs out first. Attempts
# are spaced 30s doubling to a 15 minute cap. The two hour figure comes from the
# asciidoctorj 403, where a 5 x 30s retry was not nearly enough.
#
# The deadline is wall clock, not the sum of the sleeps, because the sleeps are
# the small half. A hung install costs whatever its own timeout is - chocolatey
# defaults to 2700s - so an attempt can take three quarters of an hour while the
# sleeps between attempts add up to 45 minutes total. Bounding only the sleeps
# bounded the wrong thing: run 33111295598 got through 7 of its 13 attempts in
# six hours and was then killed by the job timeout, which reports as "cancelled"
# and skipped every downstream test job. Give up on time instead, so the number
# in this comment is the number that actually happens.
#
# Usage:
#   source "${GITHUB_ACTION_PATH}/../retry.sh"
#   install_foo() { some-package-manager install a b c; }
#   retry "foo packages" install_foo
#
# The retried unit is a function, not a string, so multi-command installs keep
# their quoting. Chain the commands inside it with && - errexit is disabled
# inside a function called from an if-condition, so a bare newline between two
# commands would let a failed first command go unnoticed.
#
# RETRY_ATTEMPT is set to the 1-based attempt number before each call, so a
# retried command can behave differently the second time around (chocolatey
# needs -f to reinstall a package a failed attempt left behind, but forcing it
# on the first attempt would re-download things that are already fine).
#
# Keep the retried unit as small as the thing that fails. Anything conditioned
# on RETRY_ATTEMPT applies to the WHOLE unit, so a batch of independent packages
# retried together means one package's transient failure changes how all the
# others are installed on the next attempt. That is not hypothetical: it is what
# turned a sourceforge 404 into the six hour hang described above.
retry() {
    local what="$1"
    shift

    local max_attempts=13
    local budget=7200
    local delay=30
    local max_delay=900
    local started elapsed remaining attempt

    started=$(date +%s)

    for attempt in $(seq 1 "$max_attempts"); do
        export RETRY_ATTEMPT="$attempt"
        if "$@"; then
            if [ "$attempt" -gt 1 ]; then
                echo "${what}: succeeded on attempt ${attempt}."
            fi
            return 0
        fi

        elapsed=$(( $(date +%s) - started ))

        if [ "$attempt" -eq "$max_attempts" ]; then
            echo "${what}: failed after ${attempt} attempts in ${elapsed}s, giving up."
            return 1
        fi

        # Stop when the next sleep would take us past the budget, rather than
        # after it: sleeping out the remainder only to give up is pure delay.
        remaining=$(( budget - elapsed ))
        if [ "$remaining" -le "$delay" ]; then
            echo "${what}: failed after ${attempt} attempts in ${elapsed}s," \
                 "which is the ${budget}s budget, giving up."
            return 1
        fi

        echo "${what}: attempt ${attempt} failed after ${elapsed}s;" \
             "retrying in ${delay}s..."
        sleep "$delay"
        delay=$((delay * 2))
        if [ "$delay" -gt "$max_delay" ]; then
            delay=$max_delay
        fi
    done
}
