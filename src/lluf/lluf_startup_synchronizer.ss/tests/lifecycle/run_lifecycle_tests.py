#!/usr/bin/env python3
# -*- coding: utf-8 -*-
###############################################################################
#
# Copyright Saab AB, 2026 (http://safirsdkcore.com)
#
###############################################################################
#
# This file is part of Safir SDK Core.
#
# Safir SDK Core is free software: you can redistribute it and/or modify
# it under the terms of version 3 of the GNU General Public License as
# published by the Free Software Foundation.
#
# Safir SDK Core is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with Safir SDK Core.  If not, see <http://www.gnu.org/licenses/>.
#
###############################################################################
"""Multi process lifecycle tests for StartupSynchronizer.

The other tests in this directory check that a lot of processes can start at the
same time. These check what happens around the *edges* of a resource's life:
processes that arrive while the last user is shutting down, processes that are
killed while they are creating the resource, processes that are killed while
using it, and long sequences of restarts. Those are the situations that used to
go wrong, and the ones that are impossible to see from a single generation test.

Each scenario is independent and uses its own resource name, so a failure in one
does not cascade. The exit code is 0 only if every scenario passed.
"""
import argparse
import os
import queue
import shutil
import subprocess
import sys
import tempfile
import threading
import time

#Generous: these are all local, and the point is to notice a hang, not to measure
#anything. ctest has its own timeout as a backstop.
DEFAULT_TIMEOUT = 60


def parse_arguments():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--test-exe", help="The lifecycle helper executable", required=True)
    parser.add_argument("--scenario", help="Only run the scenario with this name")
    return parser.parse_args()


class Failure(Exception):
    """A scenario did not behave as it should."""


#Where the helpers keep the file that stands in for the shared resource. Every
#process that uses the same resource name gets the same path, so they can all
#check that nobody replaced or removed the generation they were using. That check
#is what turns each of these scenarios into a test for the thing that actually
#matters, without having to guess at any timing.
WITNESS_DIR = None


class Helper:
    """One helper process, with its output collected by a reader thread.

    The reader thread matters: without it, waiting for a specific line from one
    process while another one is also producing output is a good way to deadlock
    on a full pipe buffer.
    """

    def __init__(self, exe, name, *extra_args, hold=True):
        self.label = f"{name}"
        command = [exe, "--name", name]
        if WITNESS_DIR:
            command.extend(["--resource", os.path.join(WITNESS_DIR, name)])
        if hold:
            command.append("--hold")
        command.extend(extra_args)

        self.proc = subprocess.Popen(command,
                                     stdout=subprocess.PIPE,
                                     stderr=subprocess.STDOUT,
                                     stdin=subprocess.PIPE,
                                     universal_newlines=True,
                                     bufsize=1)
        self.lines = []
        self.__queue = queue.Queue()
        self.__reader = threading.Thread(target=self.__read, daemon=True)
        self.__reader.start()

    def __read(self):
        for line in self.proc.stdout:
            self.__queue.put(line.rstrip("\r\n"))
        self.__queue.put(None)

    def __drain(self, timeout):
        """Get one line, remembering it. None means the process closed stdout."""
        try:
            line = self.__queue.get(timeout=timeout)
        except queue.Empty:
            raise Failure(f"{self.label}: timed out after {timeout}s waiting for output. "
                          f"Got so far: {self.lines}") from None
        if line is not None:
            self.lines.append(line)
        return line

    def wait_for(self, token, timeout=DEFAULT_TIMEOUT):
        """Wait until the process prints a line starting with token."""
        while True:
            line = self.__drain(timeout)
            if line is None:
                raise Failure(f"{self.label}: exited while waiting for '{token}'. "
                              f"Output: {self.lines}")
            if line.startswith(token):
                return line

    def release(self, timeout=DEFAULT_TIMEOUT):
        """Tell the process to let go of the resource, and wait for it to exit."""
        try:
            self.proc.stdin.write("\n")
            self.proc.stdin.flush()
        except (BrokenPipeError, OSError):
            #Already gone, which some scenarios do on purpose.
            pass
        return self.wait_for_exit(timeout)

    def wait_for_exit(self, timeout=DEFAULT_TIMEOUT):
        while self.__drain(timeout) is not None:
            pass
        try:
            self.proc.wait(timeout=timeout)
        except subprocess.TimeoutExpired:
            raise Failure(f"{self.label}: did not exit. Output: {self.lines}") from None
        return self.proc.returncode

    def kill(self):
        """Kill the process the way the operating system would, with no cleanup."""
        self.proc.kill()
        self.proc.wait()

    def events(self):
        return [line[len("EVENT "):] for line in self.lines if line.startswith("EVENT ")]

    def errors(self):
        return [line for line in self.lines if line.startswith("ERROR")]

    def result(self):
        """The created/used/destroyed counts the process reported when it exited."""
        for line in self.lines:
            if line.startswith("RESULT "):
                return dict(
                    (key, int(value))
                    for key, value in (field.split("=") for field in line.split()[1:]))
        return None


def expect(condition, message):
    if not condition:
        raise Failure(message)


def expect_result(helper, created, used, destroyed):
    result = helper.result()
    expect(result is not None, f"{helper.label}: never reported a result. Output: {helper.lines}")
    expected = {"created": created, "used": used, "destroyed": destroyed}
    expect(
        result == expected, f"{helper.label}: expected {expected} but got {result}. "
        f"Output: {helper.lines}")


def expect_clean_exit(helper, code):
    expect(code == 0, f"{helper.label}: exited with {code}. Output: {helper.lines}")
    expect(not helper.errors(), f"{helper.label}: reported errors {helper.errors()}")


####################################################################################
# The scenarios
####################################################################################


def parallel_start(exe):
    """Many processes starting at once: exactly one of them creates the resource,
    and the last one to leave destroys it."""
    count = 20
    helpers = [Helper(exe, "SS_LIFE_parallel") for _ in range(count)]
    for helper in helpers:
        helper.wait_for("READY")

    creates = sum(helper.events().count("CREATE") for helper in helpers)
    uses = sum(helper.events().count("USE") for helper in helpers)
    expect(creates == 1, f"expected exactly one CREATE among {count} processes, got {creates}")
    expect(uses == count, f"expected {count} USE callbacks, got {uses}")

    #Released one at a time, so the last one out is unambiguous.
    destroys = 0
    for helper in helpers:
        expect_clean_exit(helper, helper.release())
        destroys += helper.events().count("DESTROY")
    expect(destroys == 1, f"expected exactly one DESTROY, got {destroys}")


def simultaneous_shutdown(exe):
    """Everyone letting go at the same instant. Destroy is best effort, so nobody
    may crash and the state left behind has to be usable by the next generation,
    but it is allowed for nobody to run Destroy at all."""
    count = 8
    helpers = [Helper(exe, "SS_LIFE_simultaneous") for _ in range(count)]
    for helper in helpers:
        helper.wait_for("READY")

    for helper in helpers:
        try:
            helper.proc.stdin.write("\n")
            helper.proc.stdin.flush()
        except (BrokenPipeError, OSError):
            pass

    destroys = 0
    for helper in helpers:
        expect_clean_exit(helper, helper.wait_for_exit())
        destroys += helper.events().count("DESTROY")
    expect(destroys <= 1, f"more than one process destroyed the resource: {destroys}")

    #Whatever they left behind, a new generation has to work.
    after = Helper(exe, "SS_LIFE_simultaneous", hold=False)
    expect_clean_exit(after, after.wait_for_exit())
    expect_result(after, created=1, used=1, destroyed=1)


def latecomer_during_slow_destroy(exe):
    """A process that starts while the last user is in the middle of destroying the
    resource. It used to fail hard with "It appears that Create failed in some
    other process", and then destroy the next generation on its way out. It has to
    simply create the next generation instead."""
    leaving = Helper(exe, "SS_LIFE_latecomer", "--destroy-delay", "3000")
    leaving.wait_for("READY")

    #Let it start shutting down, and wait until it is actually inside Destroy.
    leaving.proc.stdin.write("\n")
    leaving.proc.stdin.flush()
    leaving.wait_for("EVENT DESTROY")

    latecomer = Helper(exe, "SS_LIFE_latecomer", hold=False)
    expect_clean_exit(latecomer, latecomer.wait_for_exit())
    expect_result(latecomer, created=1, used=1, destroyed=1)

    expect_clean_exit(leaving, leaving.wait_for_exit())
    expect_result(leaving, created=1, used=1, destroyed=1)


def no_destroy_while_another_process_uses_it(exe):
    """The creator leaving first must not tear the resource down under the feet of
    the processes that are still using it."""
    creator = Helper(exe, "SS_LIFE_still_in_use")
    creator.wait_for("READY")
    expect(creator.events().count("CREATE") == 1, "the first process should have created it")

    user = Helper(exe, "SS_LIFE_still_in_use")
    user.wait_for("READY")
    expect(user.events().count("CREATE") == 0, "the second process should not have created it")

    expect_clean_exit(creator, creator.release())
    expect_result(creator, created=1, used=1, destroyed=0)

    #And the remaining process is still fine, and destroys it when it goes.
    expect_clean_exit(user, user.release())
    expect_result(user, created=0, used=1, destroyed=1)


def creator_killed_while_creating(exe):
    """The worst moment to kill a process: it has been given the job of creating
    the resource, and somebody else is already waiting for it to finish."""
    creator = Helper(exe, "SS_LIFE_killed_creator", "--hang-in-create")
    creator.wait_for("EVENT HANGING_IN_CREATE")

    waiter = Helper(exe, "SS_LIFE_killed_creator")

    #Wait until the waiter is actually inside the protocol before killing the
    #creator. Without this the waiter may not even have got that far yet, and
    #would find a clean system instead of a half created resource, which is a much
    #easier thing to recover from.
    waiter.wait_for("EVENT STARTING")
    time.sleep(0.3)

    creator.kill()

    #The waiter has to notice that the resource never appeared and create it.
    waiter.wait_for("READY")
    expect(waiter.events().count("CREATE") == 1,
           f"the waiter should have created the resource, events: {waiter.events()}")
    expect_clean_exit(waiter, waiter.release())
    expect_result(waiter, created=1, used=1, destroyed=1)


def creator_exits_while_creating(exe):
    """Same thing, but the creator manages to leave on its own from inside the
    Create callback, without cleaning anything up."""
    creator = Helper(exe,
                     "SS_LIFE_exiting_creator",
                     "--create-delay",
                     "500",
                     "--exit-in-create",
                     "7",
                     hold=False)
    creator.wait_for("EVENT CREATE")

    waiter = Helper(exe, "SS_LIFE_exiting_creator")
    waiter.wait_for("EVENT STARTING")

    expect(creator.wait_for_exit() == 7, f"unexpected exit code. Output: {creator.lines}")

    waiter.wait_for("READY")
    expect(waiter.events().count("CREATE") == 1,
           f"the waiter should have created the resource, events: {waiter.events()}")
    expect_clean_exit(waiter, waiter.release())
    expect_result(waiter, created=1, used=1, destroyed=1)


def killed_user_does_not_leak_a_user(exe):
    """A killed process must not leave behind a claim on the resource, or it would
    never be destroyed again. This is the one thing file locks give us for free,
    and it is worth having a test that says so."""
    creator = Helper(exe, "SS_LIFE_killed_user")
    creator.wait_for("READY")

    user = Helper(exe, "SS_LIFE_killed_user")
    user.wait_for("READY")
    user.kill()

    #The creator is now the only user left, so it has to destroy the resource.
    expect_clean_exit(creator, creator.release())
    expect_result(creator, created=1, used=1, destroyed=1)


def rapid_sequential_restarts(exe):
    """What restarting a node looks like: the same resource created and destroyed
    over and over, with nothing accumulating and nothing failing."""
    for i in range(40):
        helper = Helper(exe, "SS_LIFE_restarts", hold=False)
        code = helper.wait_for_exit()
        expect_clean_exit(helper, code)
        expect_result(helper, created=1, used=1, destroyed=1)


def overlapping_restarts(exe):
    """The same, but each new process arrives before the old one has left, so the
    resource is handed over rather than recreated."""
    previous = Helper(exe, "SS_LIFE_overlapping")
    previous.wait_for("READY")
    expect(previous.events().count("CREATE") == 1, "the first process should have created it")

    for i in range(20):
        current = Helper(exe, "SS_LIFE_overlapping")
        current.wait_for("READY")
        expect(
            current.events().count("CREATE") == 0,
            f"process {i} created a second generation while the resource was in use: "
            f"{current.events()}")

        expect_clean_exit(previous, previous.release())
        expect(previous.events().count("DESTROY") == 0,
               f"process {i} destroyed a resource that was still in use: {previous.events()}")
        previous = current

    expect_clean_exit(previous, previous.release())
    expect(previous.events().count("DESTROY") == 1, "the last process should have destroyed it")


def an_instance_that_outlived_a_finished_resource(exe):
    """The worst of the old failure modes, from the process level.

    One process uses the resource and lets go of it again, but keeps a second,
    older instance around. Another process then takes over the resource. When the
    old instance finally starts, it must join what is there - it must not create a
    second generation of a resource that somebody else is using. That used to
    happen, because the first instance deleted the lock files on its way out and
    everything the process did afterwards was locking files that no longer had a
    name."""
    outliving = Helper(exe, "SS_LIFE_outliving", "--outlive")
    outliving.wait_for("READY_FOR_RESTART")
    expect(
        outliving.events().count("DESTROY") == 1,
        f"the inner instance should have destroyed the resource: {outliving.events()}")

    #Another process now owns the resource.
    owner = Helper(exe, "SS_LIFE_outliving")
    owner.wait_for("READY")
    expect(owner.events().count("CREATE") == 1,
           f"the second process should have created a new generation: {owner.events()}")

    #And now the old instance starts, while the owner is still using it.
    outliving.proc.stdin.write("\n")
    outliving.proc.stdin.flush()
    outliving.wait_for("READY")

    creates_after = outliving.events().count("CREATE")
    expect(
        creates_after == 1, "the outliving instance created a second live generation "
        f"(CREATE seen {creates_after} times, expected only the inner one): {outliving.events()}")

    expect_clean_exit(outliving, outliving.release())
    expect_result(outliving, created=0, used=1, destroyed=0)

    expect_clean_exit(owner, owner.release())
    expect(owner.events().count("DESTROY") == 1,
           f"the last process out should have destroyed the resource: {owner.events()}")


SCENARIOS = (
    parallel_start,
    simultaneous_shutdown,
    latecomer_during_slow_destroy,
    no_destroy_while_another_process_uses_it,
    creator_killed_while_creating,
    creator_exits_while_creating,
    killed_user_does_not_leak_a_user,
    rapid_sequential_restarts,
    overlapping_restarts,
    an_instance_that_outlived_a_finished_resource,
)


def main():
    global WITNESS_DIR
    args = parse_arguments()
    WITNESS_DIR = tempfile.mkdtemp(prefix="ss_lifecycle_")

    scenarios = SCENARIOS
    if args.scenario:
        scenarios = tuple(s for s in SCENARIOS if s.__name__ == args.scenario)
        if not scenarios:
            print("No such scenario:", args.scenario)
            print("Available:", ", ".join(s.__name__ for s in SCENARIOS))
            return 1

    failures = []
    for scenario in scenarios:
        print("---", scenario.__name__, flush=True)
        try:
            scenario(args.test_exe)
            print("   ok", flush=True)
        except Failure as exc:
            print("   FAILED:", exc, flush=True)
            failures.append(scenario.__name__)
        #pylint: disable=broad-except
        except Exception as exc:
            print("   FAILED with an unexpected error:", repr(exc), flush=True)
            failures.append(scenario.__name__)

    shutil.rmtree(WITNESS_DIR, ignore_errors=True)

    if failures:
        print("\nfailure! These scenarios failed:", ", ".join(failures))
        return 1

    print("\nsuccess")
    return 0


if __name__ == "__main__":
    sys.exit(main())
