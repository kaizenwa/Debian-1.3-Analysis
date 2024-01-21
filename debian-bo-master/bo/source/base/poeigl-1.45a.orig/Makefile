# Makefile for login program and other related utils.
# By poe@daimi.aau.dk
# All this code is freely distributable.

CC     = gcc -Wall

# Change SBINDIR to /sbin if you want admin binaries in there instead of
#	/etc. This affects the location of init and getty.
SBINDIR	= /sbin

# Some might want to change this to /etc, but then you must change
# <paths.h> too!!
#LOGDIR	= /var/log
#LOGDIR = /var/adm

# Set CAGEDIR to the root directory of a controlled, more secure environment.
# cage will do a chroot() to that directory. Preferably on a separate 
# partition
CAGEDIR	= /usr2/cage

# Add -DSHADOW_PWD to CFLAGS to compile with support for shadow passwords
# Add -DSET_TZ to make simpleinit set the timezone from /etc/TZ
# Add -DHAVE_QUOTA to include quota() checks in login.c
# Add -DCRYPTOCARD to support DES based cryptocard authentication
#     this needs a DES library like koontz-des, see cryptocard.c for
#     a pointer to sources. 
CFLAGS = -O2 -I. -DSBINDIR=\"$(SBINDIR)\" -DCAGEDIR=\"$(CAGEDIR)\" -DSHADOW_PWD -DCRYPTOCARD
# Add -N to LDFLAGS to get small, non-demand-loadable executables on a.out
# systems
LDFLAGS= -s

# Set LIBS = -lshadow if you want to support shadow passwords in login
# Add -lyp to LIBS if you want YP unless there's already YP support in your
# C library.
LIBS   = ../libdes/libdes.a

loginobj = login.o getpass.o checktty.o cryptocard.o
agettyobj = agetty.o
initobj = simpleinit.o

.SUFFIXES: .c .o
.c.o:
	${CC} ${CFLAGS} -c $*.c

all:	login who agetty init write cage hostid hostname domainname

install:
	-mv $(SBINDIR)/init $(SBINDIR)/init.old
	cp -f init $(SBINDIR)
	-mv $(SBINDIR)/agetty $(SBINDIR)/agetty.old
	cp -f agetty $(SBINDIR)
	chown root.bin $(SBINDIR)/init $(SBINDIR)/agetty
	chmod 544 $(SBINDIR)/init $(SBINDIR)/agetty
	cp -f login /bin
	chown root.bin /bin/login
	chmod a=rx,u+s /bin/login
	cp -f who hostname domainname write users mesg cage hostid /usr/bin
	-ln -s /usr/bin/write /usr/bin/wall
	# -ln -s /dev/tty0 /dev/console
#	touch $(LOGDIR)/utmp $(LOGDIR)/wtmp $(LOGDIR)/lastlog
#	chown root.bin $(LOGDIR)/utmp $(LOGDIR)/wtmp $(LOGDIR)/lastlog
#	chmod u=rw,og=r $(LOGDIR)/utmp $(LOGDIR)/wtmp $(LOGDIR)/lastlog
	chown root.bin /usr/bin/cage; chmod a=rx,u+s /usr/bin/cage

Install: install
	@echo Are you really, really sure?
	@echo If yes, do the next line by hand:
	@echo cp inittab securetty issue motd usertty /etc

install-man:
	cp *.1 /usr/man/man1
	cp *.8 /usr/man/man8
	@echo Now you should run catman to format the manual pages

init:	${initobj}
	${CC} -s -static -o init ${initobj} $(LIBS)

agetty:	${agettyobj}
	${CC} -s -static -o agetty ${agettyobj} $(LIBS)

login:	$(loginobj)
	$(CC) -s -o login $(loginobj) $(LIBS)

clean:
	rm -f *.o

real-clean:
	rm -f *.o login who agetty hostname domainname init write cage hostid
