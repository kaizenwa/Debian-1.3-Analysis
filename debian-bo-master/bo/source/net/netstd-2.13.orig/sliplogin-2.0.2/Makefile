CC = gcc
LINK = $(CC)
CFLAGS = -O2 -pipe -fomit-frame-pointer -m486 #-Wall
DEFINES = #-DFUNMESSAGE #-DOLDMESSAGE
LDFLAGS = -s

SRC = sliplogin.c statistic.c scanfile.c translate.c
OBJ = $(SRC:.c=.o)
LIBS = #-lshadow

#
#You should really edit /etc/group to limit access to sliplogin
#otherwise sliplogin has to be executable for the world.
#
SLIPGROUP = slip

TARGETS = sliplogin 

all: $(TARGETS)
	@echo done

sliplogin: $(OBJ) 
	$(LINK) $(OBJ) $(LIBS) $(LDFLAGS) -o $@

clean:
	rm -f $(OBJ) core 

install: sliplogin
	install -m4754 sliplogin /sbin
	install -m644 sliplogin.8 /usr/man/man8
	chgrp $(SLIPGROUP) /sbin/sliplogin
	@echo "Don't forget to edit slip.hosts, slip.login and slip.logout"
	@echo "and copy these files to the proper place (/etc)."

depend:
	makedepend $(SRC)

ci:
	ci -m"I hate log messages!" -t-"I hate description prompts!" -l $(SRC) Makefile

co:
	co $(SRC) Makefile

.c.o: 
	$(CC) $(CFLAGS) $(DEFINES) -c $<

