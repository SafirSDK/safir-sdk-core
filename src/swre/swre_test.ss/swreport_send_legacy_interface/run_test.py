from __future__ import print_function
import subprocess, os, time, sys
try:
    import ConfigParser
except ImportError:
    import configparser as ConfigParser
import socket
import shutil
import re
import argparse

parser = argparse.ArgumentParser()
parser.add_argument("test_conf_dir", help="Test configuration directory")
args = parser.parse_args()

if sys.platform == "win32":
    config_type = os.environ.get("CMAKE_CONFIG_TYPE")
    exe_path = config_type if config_type else ""
else:
    exe_path = "."

test_pgm = "swreport_systemlog_sender"  
test_pgm_path = os.path.join(exe_path, test_pgm)

conf_dir = os.path.join(args.test_conf_dir, "syslog_and_native_logging")
conf_file = os.path.join(conf_dir, "logging.ini")

os.environ["SAFIR_TEST_CONFIG_OVERRIDE"] = conf_dir

config = ConfigParser.ConfigParser()
      
if not config.read(conf_file):
    print("Failed to read file " + conf_file)
    sys.exit(1)
    
syslog_server_address = config.get('SYSTEM-LOG','syslog-server-address')
syslog_server_port = config.get('SYSTEM-LOG','syslog-server-port')

#Start listen to messages sent to the syslog server port
sock = socket.socket(socket.AF_INET, # Internet
                     socket.SOCK_DGRAM ) # UDP
sock.settimeout(2)
sock.bind((syslog_server_address, int(syslog_server_port)))
    
#Run the program that sends system logs
proc = subprocess.Popen(test_pgm_path, stdout=subprocess.PIPE, stderr=subprocess.STDOUT,universal_newlines=True)
stdout, stderr = proc.communicate()
print (stdout)

if proc.returncode != 0:
    print("Failed when sending legacy swre logs!")
    sys.exit(1)

try:
    log_common_part = r"\D{3} [ |\d]\d \d{2}:\d{2}:\d{2} \S* " + system_log_test_pgm + r"\[\d*\]: "
    for test in range(5):
        data, addr = sock.recvfrom( 10 * 1024 ) # buffer size is 10 k
        print ("Received data: " + data) 
        if test == 0:
            pri = r"<8>"
            text = r"This is an emergency log"
        elif test == 1:
            pri = r"<9>"
            text = r"This is an alert log"            
        elif test == 2:
            pri = r"<10>"
            text = r"This is a critical log with \n newline and \t tab"
        elif test == 3:
            pri = r"<11>"
            text = r"This is an error log"
        elif test == 4:
            pri = r"<12>"
            text = r"This is a warning log with \n newline and \t tab"          

        p = re.compile(pri + log_common_part + text)
        data = data.decode("utf-8")
        if p.match(data) == None:
            print ("Unexpected syslog message: " + data)
            sys.exit(1)
            
except:
    print("Timeout")
    sys.exit(1)

sys.exit(0)


