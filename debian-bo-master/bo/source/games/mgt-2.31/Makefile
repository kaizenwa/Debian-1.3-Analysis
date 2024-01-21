#   There is NO X version of this program
#
#        "mgt" Copyright 1991 Shodan
#		All Rights Reserved.
#		Program by Greg Hale and Adrian Mariano
#
#Permission to use, copy, modify, and distribute this software and its
#documentation for any purpose and without fee is hereby granted,
#provided that this entire comment and copyright notice appear in all
#copies and that both that copyright notice and this permission notice
#appear in supporting documentation.  No representations are made about
#the suitability of this software for any purpose.  It is provided "as
#is" without express or implied warranty.
#
#Please send copies of extensions to:
#
# hale@scam.berkeley.edu
# 128.32.138.4    scam.berkeley.edu
#
#Donations for the 'From My Go Teacher' series may be sent to:
#	Shodan
#	P.O. Box 4456
#	Berkeley, CA 94704
#	(415) 574-5572
#
#
# Makefile for mgt
# by Greg Hale and Adrian Mariano
#
# *** C compile flags.
#
# -O: optimize
# -DDEBUG: turn on program debugging
#
# *** CONFIGURATION ***
#
# Choose one of
#   -DMGT_UNIX  Compile under UNIX
#   -DMGT_IBM   Compile on IBM PC under Turbo C (Use the other Makefile!)
#
#   -DMGT_LIB=\"/usr/lib/mgt/\":   defines a library directory for games
#

DEFS=-DMGT_UNIX -DMGT_LIB=\"/usr/lib/games/mgt/\"

# If you read this Makefile, you may notice many references to an 
# X Windows program.  This program has been released and is available
# at the standard archive sites.


E	=
O 	= .o

CC	= cc
CFLAGS	= -O $(DEFS)

LD 	= cc
LDFLAGS	=
LIBS	= 
#CUR_LIBS = -lcurses -ltermcap
CUR_LIBS = -lcurses
X11_LIBS =

# DIRECTORIES - CONFIGURE DESTINATIONS
# BINDIR is where it is installed
BINDIR		=		/usr/games
MAN             =		mgt.doc
SHAR 		=		mgt.sh

# files which make up the shell archive along with sources

FILES = 	Smart-Go.def README Makefile format mgt.6 \
		Spec.io Makefile.bc Build.com mailgo mailgo.6 mgtdoc.asc \
		Sample.01 Sample.02 Rules Prop.lst README.IBM wrapmgt.6 \
		wrapmgt.c mgt2short mgt2short.6 mgtshort.bat mgtshort.cmd

SH_OBJS = 	help$O build$O comment$O doit$O \
		edit$O mgt$O parse$O play$O tree$O

SH_SRCS =	help.c ascii.c build.c comment.c doit.c \
		edit.c mgt.c parse.c play.c tree.c

ASC_OBJS= ascii$O

OBJECTS =	help$O ascii$O build$O comment$O doit$O \
		edit$O mgt$O parse$O play$O tree$O


SRCS =		help.c asc_ibm.inc asc_unix.inc mou.c \
		ascii.c build.c comment.c doit.c edit.c \
		mgt.c parse.c play.c tree.c

HDRS = 		mgt.h proto.h mou.h


all : begin curses wrapmgt man end

begin :
	@echo Begin...

end :
	@echo ...End
		
curses : mgt$E
	@echo Completed curses based version.

mgt$E : $(SH_OBJS) $(ASC_OBJS)
	$(LD) $(LDFLAGS) -o mgt$E $(SH_OBJS) $(ASC_OBJS) $(CUR_LIBS)

wrapmgt$E : wrapmgt.o
	$(LD) -o wrapmgt$E wrapmgt.o

ascii.o: ascii.c asc_unix.inc

$(MAN): mgt.6
	@echo "Creating mgt manual..."
	@nroff -man mgt.6 > $(MAN)
	@echo "done."

mailgo.doc: mailgo.6
	@echo "Creating mailgo manual..."
	@nroff -man mailgo.6 > mailgo.doc
	@echo "done."

wrapmgt.doc: wrapmgt.6
	@echo "Creating wrapmgt manual..."
	@nroff -man wrapmgt.6 > wrapmgt.doc
	@echo "done."

mgt2short.doc: mgt2short.6
	@echo "Creating mgt2short manual..."
	@nroff -man mgt2short.6 > mgt2short.doc
	@echo "done."

man: $(MAN) wrapmgt.doc mailgo.doc mgt2short.doc

clean:
	@echo "removing extra files..."
	@-rm -f *$O $(SHAR) xmgt$E mgt$E
	@echo "done."

proto:
	@echo "making proto.h file..."
	@-rm -f ./proto.h
	@egrep "FUNCTION" $(SH_SRCS) | sed "s/(.*)/();/" | sed "s/^.*://" | sed "s/FUNCTION/extern/"  | cat > ./proto.h
	@echo "done."

lint:
	@echo "making lint file..."
	@-rm -f ./lint.out
	@lint -u $(DEFS) $(SH_SRCS) > ./lint.out
	@echo "done."

new: proto clean all

mgtdoc.asc: $(MAN)
	@echo "Making ascii doc file..."
	@cat $(MAN) | col -b > mgtdoc.asc;
	@echo "done."


shar: mgtdoc.asc
	@shar $(SRCS) $(HDRS) $(FILES) > $(SHAR)

install : $(PROGRAM) $(ARCHIVE)
	@-install $(PROGRAM) $(BINDIR)/$(PROGRAM)

