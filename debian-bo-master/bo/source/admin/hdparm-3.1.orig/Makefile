# Makefile for hdparm
 
all: hdparm

hdparm: hdparm.c /usr/include/linux/hdreg.h
	gcc -Wall -Wstrict-prototypes -O2 -s -o hdparm hdparm.c
 
install: all hdparm.8
	if [ -f /usr/sbin/hdparm ]; then rm -f /usr/sbin/hdparm ; fi 
	if [ -f /usr/local/bin/hdparm ]; then rm -f /usr/local/bin/hdparm ; fi 
	if [ -f /usr/local/sbin/hdparm ]; then rm -f /usr/local/sbin/hdparm ; fi 
	if [ -f /usr/man/man8/hdparm.8 ]; then rm -f /usr/man/man8/hdparm.8 ; fi
	if [ -f /usr/local/man/man8/hdparm.8 ]; then rm -f /usr/local/man/man8/hdparm.8 ; fi 
	install -m 755 -o root -g root hdparm /usr/local/sbin
	install -m 644 -o root -g root hdparm.8 /usr/local/man/man8

clean:
	rm -f hdparm *.o core
