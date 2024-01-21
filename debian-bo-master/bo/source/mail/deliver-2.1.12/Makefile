# $Id: Makefile,v 1.15 1993/10/28 16:48:43 chip Exp $
#
# Makefile for deliver
#
#  +----------------+
#  | Things to make |
#  +----------------+
#
#       deliver         Compile and link the deliver program.
#       header          Compile and link the header program.
#       install         Install deliver and header.  (You must be root.)
#       lint            Run lint on all sources, creating lint.out.
#       shar            Create distribution sharchives.
#       clean           Clean up.
#       clobber         Remove everything that can be regenerated.
#
#  +---------------+
#  | Configuration |
#  +---------------+
#
# SHELL
#       I don't have to tell you...
#
# COPY
#       Your local copy program.  Users of SCO UNIX or Xenix may want to
#       change this to "copy -m" which preserves file modification time.
#
# CC
#       Your favorite C compiler.
#
# DELHOME
#	Base directory for Deliver installation, if any.  A typical value
#	is "/usr/deliver".  If you define DELHOME, uncomment the definition
#	of HDEFS that follows it.
#
# OSHEADER
#       Name of OS configuration header file in "conf" directory.
#
# UDEFS
#       User-added flags for cc and lint.
#
#       * If your compiler is ANSI but doesn't define __STDC__,
#         include "-DANSI_C".
#
# DEFS
#       Flags for cc and lint.
#
# CFLAGS
#	Compile-time flags for cc; should include DEFS.
#
# LDFLAGS
#       Link-time flags for cc.  The -i flag creates pure (sharable) code.
#
# LIBS
#       Depending on your environment, you may or may not need to link
#       with "-lx".  SCO Xenix System V needs it; Altos Xenix doesn't.
#
# LINT
#	Your favorite lint program, with flags.
#
# BIN
#       Target directory for installation; /usr/bin is recommended.
#       You may use /usr/local/bin (or whatever), but if you do,
#       be sure that the directory you choose is in your SAFEPATH
#       as defined in config.h.
#
# GROUP
#	Name of group that should own the installed Deliver binary.
#
# SHAR
#       Your local sharchive generator.  (OPTIONAL)
#
# DELSHAR
#       Basename of sharchives created by "make shar".  (OPTIONAL)
#

SHELL	= /bin/sh
COPY	= cp
CC	= cc
#DELHOME = /usr/deliver
#HDEFS	= -DDELHOME=\"$(DELHOME)\"
UDEFS	=
OSHEADER = os-dummy.h
DEFS	= -Iconf -DOSINCLUDE=\"$(OSHEADER)\" $(HDEFS) $(UDEFS)
CFLAGS	= -O $(DEFS)
LDFLAGS	=
LIBS	=
LINT	= lint -x
BIN	= /usr/bin
#BIN	= $(DELHOME)/bin
GROUP	= root
SHAR	= shar
DELSHAR	= deliver

####################################################
##  YOU SHOULD NOT HAVE TO EDIT BELOW THIS POINT  ##
####################################################

#
# The files that make up the deliver distribution.
#

DOCS	= README INSTALL SENDMAIL MMDF deliver.8
MF	= Makefile

DELHDRS	= config.h context.h deliver.h dest.h patchlevel.h misc.h
ALLCONF	= conf conf/*.h

DELSRC1 = addr.c context.c
DELSRC2 = copymsg.c debug.c dest.c dfile.c lock.c
DELSRC3 = log.c main.c mbox.c
DELSRC4 = procs.c subs.c sysdep.c uucp.c
DELYY   = unctime.y
DELYC   = unctime.c
DELSRCY = $(DELSRC1) $(DELSRC2) $(DELSRC3) $(DELSRC4) $(DELYY)
DELSRCC = $(DELSRC1) $(DELSRC2) $(DELSRC3) $(DELSRC4) $(DELYC)
UIDSRCS = uid.c
HDRSRCS = header.c
COMSRCS = getopt.c
SAMPLES = samples samples/README samples/[a-z]*

DELOBJS = addr.o context.o copymsg.o debug.o dest.o dfile.o lock.o \
	  log.o main.o mbox.o procs.o subs.o sysdep.o unctime.o uucp.o
UIDOBJS = uid.o
HDROBJS = header.o
COMOBJS = getopt.o

CONFIG	= config.h conf/$(OSHEADER) conf/finish-os.h conf/local.h

#
# For GNU Make.  Sorry about the ugliness.
#

.PHONY: all install lint shar clean clobber

#
# The default target.
#

all: deliver header uid

#
# "make clobber" implies "make clean".
#

clobber:: clean

#
# How to install deliver and associated utilities.
#

install: deliver header uid
	@set X `./uid -uU | fgrep '(root)' | wc -l`; \
	case "$$2" in \
	  2) ;; \
	  *)  echo "Sorry!  You must be root to install deliver."; exit 1 ;; \
	 esac
	@h="$(DELHOME)"; \
	 case "$$h" in \
	  ?*)	for d in $$h $$h/bin $$h/lib $$h/log; \
		do \
			test -d $$d && continue; \
			( set -x; \
			  mkdir $$d || exit 1; \
			  chgrp $(GROUP) $$d; \
			  chmod 755 $$d ) || exit 1; \
		done ;; \
	 esac
	rm -f $(BIN)/deliver.O
	-mv $(BIN)/deliver $(BIN)/deliver.O
	$(COPY) deliver $(BIN)/deliver
	chgrp $(GROUP) $(BIN)/deliver
	chown root $(BIN)/deliver
	chmod 4711 $(BIN)/deliver
	rm -f $(BIN)/header.O
	-mv $(BIN)/header $(BIN)/header.O
	$(COPY) header $(BIN)/header
	chmod 755 $(BIN)/header

#
# How to compile and link the program.
#

deliver: $(DELOBJS) $(COMOBJS)
	$(CC) $(LDFLAGS) -o $@ $(DELOBJS) $(COMOBJS) $(LIBS)

$(DELOBJS): $(CONFIG) $(DELHDRS)
unctime.o: unctime.c

clean::
	rm -f $(DELOBJS) $(DELYC)
clobber::
	rm -f deliver

#
# A header parsing program.
#

header: $(HDRSRCS) $(COMOBJS)
	$(CC) $(CFLAGS) $(LDFLAGS) -o $@ $(HDRSRCS) $(COMOBJS) $(LIBS)
	rm -f $(HDROBJS)

clobber::
	rm -f header

#
# A little program to check on user and group id's.
# (I wish that the System V "id" program were available everywhere.)
#

uid: $(CONFIG) $(UIDSRCS) $(COMOBJS)
	$(CC) $(CFLAGS) $(LDFLAGS) -o $@ $(UIDSRCS) $(COMOBJS) $(LIBS)
	rm -f $(UIDOBJS)

clobber::
	rm -f uid

#
# Common subroutines
#

$(COMOBJS): $(CONFIG)

clean::
	rm -f $(COMOBJS)

#
# Look for fuzz.
#

lint: deliver.lint header.lint uid.lint

deliver.lint: $(CONFIG) $(DELHDRS) $(DELSRCC) $(COMSRCS)
	$(LINT) $(DEFS) $(DELSRCC) $(COMSRCS) $(LIBS) >$@

header.lint: $(HDRSRCS) $(COMSRCS)
	$(LINT) $(DEFS) $(HDRSRCS) $(COMSRCS) $(LIBS) >$@

uid.lint: $(CONFIG) $(UIDSRCS) $(COMSRCS)
	$(LINT) $(DEFS) $(UIDSRCS) $(COMSRCS) $(LIBS) >$@

clean::
	rm -f *.lint

#
# Make distribution sharchives.
#

shar:   $(DELSHAR).01 $(DELSHAR).02 $(DELSHAR).03 $(DELSHAR).04 $(DELSHAR).05
$(DELSHAR).01: $(DOCS) $(MF) $(UIDSRCS) $(HDRSRCS) $(COMSRCS) $(SAMPLES)
	$(SHAR) >$@ $(DOCS) $(MF) $(UIDSRCS) $(HDRSRCS) $(COMSRCS) $(SAMPLES)
$(DELSHAR).02: $(ALLCONF) $(DELHDRS) $(DELSRC1)
	$(SHAR) >$@ $(ALLCONF) $(DELHDRS) $(DELSRC1)
$(DELSHAR).03: $(DELSRC2)
	$(SHAR) >$@ $(DELSRC2)
$(DELSHAR).04: $(DELSRC3)
	$(SHAR) >$@ $(DELSRC3)
$(DELSHAR).05: $(DELSRC4) $(DELYY)
	$(SHAR) >$@ $(DELSRC4) $(DELYY)

clobber::
	rm -f $(DELSHAR).??
