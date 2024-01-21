#! /bin/sh
# This script can be run under inetd at some port (telnet?:-), and 
# telnet to this will give a login prompt.
# Blatantly anti-social behaviour; we take advantage of the BSD telnet
# client behaviour to get it to do exactly what we want while ignoring
# it entirely.
# Mark Moraes, University of Toronto
PATH=/local/bin:/bin
export PATH

# first send telnet negotiation to set "mode char" i.e
# IAC WILL SUPPRESS-NO-GA IAC WILL ECHO IAC DONT ECHO
awk 'END {
	printf "%c%c%c%c%c%c%c%c%c", 255, 251, 3, 255, 251, 1, 255, 254, 1;
}' /dev/null

# now ignore the telnet client and give them a login
stelnet | onpty login
