#!/usr/bin/env python3
# -*- coding: utf-8 -*-
###############################################################################
#
# Copyright Saab AB, 2013-2014 (http://safirsdkcore.com)
#
# Created by: Anders Widén (anders.widen@consoden.se)
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
from inspect import currentframe
import subprocess, os, time, sys, datetime

if sys.platform == "win32":
    config_type = os.environ.get("CMAKE_CONFIG_TYPE")
    exe_path = config_type if config_type else ""
else:
    exe_path = "."


def log(*args, **kwargs):
    print(datetime.datetime.now().isoformat(), ":", *args, **kwargs)
    sys.stdout.flush()


def kill(proc):
    try:
        proc.kill()
    except:
        pass


def test_failed(error_str, sourceline=None):
    if sourceline is None:
        sourceline = currentframe().f_back.f_lineno

    log("Test failed at line " + str(sourceline) + "!")
    log(error_str)

    kill(subscriber1)
    kill(subscriber2)
    kill(publisher)

    sys.exit(1)


def wait_for_output(proc, output, n=1, exact_match=False):
    log("Waiting for output: \'" + output + "\' from PID:", str(proc.pid))
    no_found = 0
    while True:
        line = proc.stdout.readline().decode("utf-8")
        log(line)
        if line.find(output) != -1:
            no_found = no_found + 1
            if no_found >= n:
                break
        else:
            if exact_match:
                # didn't receive the expected line
                test_failed("Expected output: '" + output + "' Got: '" + line + "'", currentframe().f_back.f_lineno)


def wait_for_outputs(proc, outputs):
    unmatched_outputs = list(outputs)
    log("Waiting for outputs: " + str(unmatched_outputs) + " from PID:", str(proc.pid))
    while len(unmatched_outputs) > 0:
        line = proc.stdout.readline().decode("utf-8")
        log(line)
        if line.strip() in unmatched_outputs:
            unmatched_outputs.remove(line.strip())


def send_cmd(proc, cmd):
    proc.stdin.write(cmd.encode("utf-8"))
    proc.stdin.flush()


def terminate(proc):
    proc.kill()
    proc.communicate()


def exit_process(proc):
    send_cmd(proc, "EXIT\n")
    proc.communicate()


IpcPublisher = os.path.join(exe_path, "IpcPublisher")
IpcSubscriber = os.path.join(exe_path, "IpcSubscriber")

#######
log("Test that the publisher is notified of subscriber connect/disconnect")
#######
# Start a publisher that sends no messages
publisher = subprocess.Popen((IpcPublisher, "--cmd-from-stdin"),
                             stdin=subprocess.PIPE,
                             stdout=subprocess.PIPE,
                             stderr=subprocess.STDOUT)
send_cmd(publisher, "START\n")
wait_for_output(publisher, "Publisher is started", exact_match=True)

# Start two subscribers
subscriber1 = subprocess.Popen((IpcSubscriber, "--cmd-from-stdin"),
                               stdin=subprocess.PIPE,
                               stdout=subprocess.PIPE,
                               stderr=subprocess.STDOUT)
subscriber2 = subprocess.Popen((IpcSubscriber, "--cmd-from-stdin"),
                               stdin=subprocess.PIPE,
                               stdout=subprocess.PIPE,
                               stderr=subprocess.STDOUT)

# Let them connect
send_cmd(subscriber1, "CONNECT\n")
send_cmd(subscriber2, "CONNECT\n")

wait_for_output(publisher, "A Subscriber connected", exact_match=True)
wait_for_output(publisher, "A Subscriber connected", exact_match=True)

# Disconnect one of the subscribers
send_cmd(subscriber1, "DISCONNECT\n")
wait_for_output(subscriber1, "Disconnected from publisher")

# Send a message to trigger the disconnect indication.
send_cmd(publisher, "SEND Kalle 1 0\n")

wait_for_output(publisher, "A Subscriber disconnected")

# Connect subscriber again
send_cmd(subscriber1, "CONNECT\n")
wait_for_output(publisher, "A Subscriber connected", exact_match=True)

# Disconnect both subscribers
send_cmd(subscriber1, "DISCONNECT\n")
send_cmd(subscriber2, "DISCONNECT\n")
wait_for_output(subscriber1, "Disconnected from publisher")
wait_for_output(subscriber2, "Disconnected from publisher")

# Send a message to trigger the disconnect indication.
send_cmd(publisher, "SEND Kalle 1 0\n")

wait_for_output(publisher, "A Subscriber disconnected", exact_match=True)
wait_for_output(publisher, "A Subscriber disconnected", exact_match=True)

terminate(publisher)
terminate(subscriber1)
terminate(subscriber2)

#######
log("Test that the publisher can be started before the subscribers")
#######
# Start a publisher that sends messages
publisher = subprocess.Popen((IpcPublisher, "--message-delay", "10"), stdout=subprocess.PIPE, stderr=subprocess.STDOUT)
wait_for_output(publisher, "Publisher is started")

# Start two subscribers that receives 5 messages and then exit
subscriber1 = subprocess.Popen((IpcSubscriber, "--number-of-messages", "5"),
                               stdout=subprocess.PIPE,
                               stderr=subprocess.STDOUT)
subscriber2 = subprocess.Popen((IpcSubscriber, "--number-of-messages", "5"),
                               stdout=subprocess.PIPE,
                               stderr=subprocess.STDOUT)

sub1_result = subscriber1.communicate()[0]
sub2_result = subscriber2.communicate()[0]

if sub1_result.decode("utf-8").count("Kalle") != 5:
    test_failed("Subscriber 1 didn't receive the expected 5 messages!")

if sub2_result.decode("utf-8").count("Kalle") != 5:
    test_failed("Subscriber 1 didn't receive the expected 5 messages!")

terminate(publisher)

#######
log("Test that subscribers can be started before publisher")
#######
subscriber1 = subprocess.Popen((IpcSubscriber, "--number-of-messages", "5"),
                               stdout=subprocess.PIPE,
                               stderr=subprocess.STDOUT)
wait_for_output(subscriber1, "Trying to connect")
subscriber2 = subprocess.Popen((IpcSubscriber, "--number-of-messages", "5"),
                               stdout=subprocess.PIPE,
                               stderr=subprocess.STDOUT)
wait_for_output(subscriber2, "Trying to connect")

# Start Publisher and wait for the subscribers to receive the messages
publisher = subprocess.Popen((IpcPublisher, "--message-delay", "10"), stdout=subprocess.PIPE, stderr=subprocess.STDOUT)

wait_for_outputs(
    publisher,
    ["A Subscriber connected!", "A Subscriber connected!", "A Subscriber disconnected!", "A Subscriber disconnected!"])

sub1_result = subscriber1.communicate()[0]
sub2_result = subscriber2.communicate()[0]

if sub1_result.decode("utf-8").count("Kalle") != 5:
    test_failed("Subscriber 1 didn't receive the expected 5 messages!")

if sub2_result.decode("utf-8").count("Kalle") != 5:
    test_failed("Subscriber 2 didn't receive the expected 5 messages!")

terminate(publisher)

#######
log("Test that subscribers can connect/disconnect several times")
#######
# Connect/disconnect witout an existing publisher
subscriber = subprocess.Popen((IpcSubscriber, "--cmd-from-stdin"),
                              stdin=subprocess.PIPE,
                              stdout=subprocess.PIPE,
                              stderr=subprocess.STDOUT)
send_cmd(subscriber, "CONNECT\n")
wait_for_output(subscriber, "Trying to connect", exact_match=True)
send_cmd(subscriber, "DISCONNECT\n")
wait_for_output(subscriber, "Disconnected from publisher", exact_match=True)
terminate(subscriber)

# Launch a publisher and wait for it to start
publisher = subprocess.Popen((IpcPublisher, "--message-delay", "10"), stdout=subprocess.PIPE, stderr=subprocess.STDOUT)
wait_for_output(publisher, "Publisher is started")

# Start a subscriber that run the loop (connect, receive 2 messages, disconnect) two times.
subscriber = subprocess.Popen((IpcSubscriber, "--cmd-from-stdin"),
                              stdin=subprocess.PIPE,
                              stdout=subprocess.PIPE,
                              stderr=subprocess.STDOUT)
send_cmd(subscriber, "CONNECT\n")
wait_for_output(subscriber, "Connected to publisher")

wait_for_output(subscriber, "Kalle", exact_match=True)
wait_for_output(subscriber, "Kalle", exact_match=True)
send_cmd(subscriber, "DISCONNECT\n")
wait_for_output(subscriber, "Disconnected from publisher")
send_cmd(subscriber, "CONNECT\n")
wait_for_output(subscriber, "Connected to publisher")
wait_for_output(subscriber, "Kalle", exact_match=True)
wait_for_output(subscriber, "Kalle", exact_match=True)
send_cmd(subscriber, "DISCONNECT\n")
wait_for_output(subscriber, "Disconnected from publisher")

terminate(publisher)
terminate(subscriber)

#######
log("Test that a publisher can do several start/stop")
#######
subscriber1 = subprocess.Popen((IpcSubscriber), stdout=subprocess.PIPE, stderr=subprocess.STDOUT)
wait_for_output(subscriber1, "Trying to connect", exact_match=True)

publisher = subprocess.Popen((IpcPublisher, "--cmd-from-stdin"),
                             stdin=subprocess.PIPE,
                             stdout=subprocess.PIPE,
                             stderr=subprocess.STDOUT)
send_cmd(publisher, "START\n")
wait_for_output(publisher, "Publisher is started", exact_match=True)
wait_for_output(publisher, "A Subscriber connected", exact_match=True)
wait_for_output(subscriber1, "Connected to publisher", exact_match=True)
send_cmd(publisher, "SEND Kalle 5 10\n")
wait_for_output(subscriber1, "Kalle", 5, exact_match=True)
send_cmd(publisher, "STOP\n")
wait_for_output(publisher, "Publisher is stopped", exact_match=True)
wait_for_output(subscriber1, "Publisher disconnected", exact_match=True)

send_cmd(publisher, "START\n")
wait_for_output(publisher, "Publisher is started", exact_match=True)
wait_for_output(publisher, "A Subscriber connected", exact_match=True)
wait_for_output(subscriber1, "Connected to publisher", exact_match=True)
send_cmd(publisher, "SEND Olle 5 10\n")
wait_for_output(subscriber1, "Olle", 5, exact_match=True)
send_cmd(publisher, "STOP\n")
wait_for_output(publisher, "Publisher is stopped", exact_match=True)
wait_for_output(subscriber1, "Publisher disconnected", exact_match=True)

terminate(publisher)
terminate(subscriber1)

#######
log("Test with large messages")
#######
subscriber1 = subprocess.Popen((IpcSubscriber, "--number-of-messages", "2", "-o"),
                               stdout=subprocess.PIPE,
                               stderr=subprocess.STDOUT)
wait_for_output(subscriber1, "Trying to connect")
subscriber2 = subprocess.Popen((IpcSubscriber, "--number-of-messages", "2", "-o"),
                               stdout=subprocess.PIPE,
                               stderr=subprocess.STDOUT)
wait_for_output(subscriber2, "Trying to connect")

# Start Publisher and send 100 Mb messages
publisher = subprocess.Popen((IpcPublisher, "--message-delay", "1000", "--large-message-size", "100000000"),
                             stdout=subprocess.PIPE,
                             stderr=subprocess.STDOUT)
wait_for_output(publisher, "A Subscriber disconnected")
wait_for_output(publisher, "A Subscriber disconnected")

sub1_result = subscriber1.communicate()[0]
sub2_result = subscriber2.communicate()[0]

if sub1_result.decode("utf-8").count("Received msg with size 100000000") != 2:
    test_failed("Subscriber 1 didn't receive the expected 2 large messages!")

if sub2_result.decode("utf-8").count("Received msg with size 100000000") != 2:
    test_failed("Subscriber 2 didn't receive the expected 2 large messages!")

terminate(publisher)

#######
log("Test that publisher and subscriber can be stopped properly")
#######
publisher = subprocess.Popen((IpcPublisher, "--cmd-from-stdin"),
                             stdin=subprocess.PIPE,
                             stdout=subprocess.PIPE,
                             stderr=subprocess.STDOUT)
send_cmd(publisher, "START\n")
wait_for_output(publisher, "Publisher is started", exact_match=True)

subscriber = subprocess.Popen((IpcSubscriber, "--cmd-from-stdin"),
                              stdin=subprocess.PIPE,
                              stdout=subprocess.PIPE,
                              stderr=subprocess.STDOUT)

send_cmd(subscriber, "CONNECT\n")

wait_for_output(publisher, "A Subscriber connected", exact_match=True)

exit_process(publisher)
exit_process(subscriber)

log("success")
sys.exit(0)
