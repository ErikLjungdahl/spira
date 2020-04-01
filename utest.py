import subprocess
import sys

p = subprocess.Popen(['cat'], stdin = subprocess.PIPE)

line = sys.stdin.readline()

####################
# Insert work here #
####################

line = line.upper()

####################

p.communicate(line)