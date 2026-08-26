# Shared retry wrapper for the package installs in this action.
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
# 13 attempts, 30s doubling to a 15 minute cap, i.e. roughly two hours of
# trying before the job gives up. That figure comes from the asciidoctorj 403,
# where a 5 x 30s retry was not nearly enough.
#
# Usage:
#   source "${GITHUB_ACTION_PATH}/retry.sh"
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
retry() {
    local what="$1"
    shift

    local max_attempts=13
    local delay=30
    local max_delay=900
    local attempt

    for attempt in $(seq 1 "$max_attempts"); do
        export RETRY_ATTEMPT="$attempt"
        if "$@"; then
            if [ "$attempt" -gt 1 ]; then
                echo "${what}: succeeded on attempt ${attempt}."
            fi
            return 0
        fi

        if [ "$attempt" -eq "$max_attempts" ]; then
            echo "${what}: failed after ${attempt} attempts, giving up."
            return 1
        fi

        echo "${what}: attempt ${attempt} failed; retrying in ${delay}s..."
        sleep "$delay"
        delay=$((delay * 2))
        if [ "$delay" -gt "$max_delay" ]; then
            delay=$max_delay
        fi
    done
}
