#!/usr/bin/env python3
import sys
import socket

if __name__ == "__main__":
    if len(sys.argv) < 3 or sys.argv[1] not in ("up", "down"):
        print("usage: communication_network up|down safir_instance")
        print("       Note: The environment variable SAFIR_COM_NETWORK_SIMULATION must be defined for the command to have any effect.")
        sys.exit("Invalid command line!") 

    cmd = ' '.join(sys.argv[1:])
    sock = socket.socket(socket.AF_INET, socket.SOCK_DGRAM)
    sock.sendto(bytes(cmd, "utf-8"), ("239.6.6.6", 16666))
