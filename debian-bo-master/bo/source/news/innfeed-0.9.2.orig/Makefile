# -*- makefile -*-
#
# Author:       James Brister <brister@vix.com> -- berkeley-unix --
# Start Date:   Wed Nov 29 22:01:56 1995
# Project:      INN (innfeed)
# File:         Makefile
# RCSId:        $Id: Makefile,v 1.25 1996/11/28 18:03:01 brister Exp $
# Description:  Makefile for innfeed.
# 


# The compiler you want. NOTE... AIX 3.2.5 users do NOT use CC. See the 
# comment in sysconfig.h
CC		= gcc

# Debug option for compiler -g or -O (or both if using gcc).
DBFLAGS		= -g

# Add any architecture specific defines, flags etc. you need
OSFLAGS		=
# OSFLAGS	= -DSOLARIS			# uncomment for Solaris

# Any extra non-architecture specific defines you may need.
#
# Add -DUSE_INN_INCLUDES to get INN headers files used (see INCDIRS below)
DEFINES		= 

# Any system specific libraries
LIBRARIES	=
# LIBRARIES	= -lnsl -lsocket -lresolv	# uncomment for SOLARIS

## Add any `-L' flags here
LDFLAGS		= 

## Add any local includes '-I' needed here. (e.g. if using -DUSE_INN_INCLUDES)
# INCDIRS	= -I/usr/local/src/inn-1.4/include
INCDIRS		= 

## Uncomment the next line to build in larry wall's version of malloc. NOTE: 
## You need to build malloc.o with a -I pointing at the include subdirectory
## of your inn source tree. See INCDIRS above.
# MALLOC	= malloc.o

# Uncomment the next line if you don't have a inet_aton() in your
# libraries (Solaris is one such OS).
# EXTRA_SRC	= inet_addr.c

# Any extra compiler flags for warning generation.
WARNINGS	=
# WARNINGS	= -Wall $(ALL_GCC_WARNINGS)

## Uncomment the next three lines to use the GNU dmalloc library.
#DMALLOC		= -DUSE_DMALLOC
#DMALLOC_LIB	= -L../dmalloc-3.1.3 -ldmalloc 
#DMALLOC_INCDIR	= -I../dmalloc-3.1.3

LINTFLAGS	= -b -h -z $(DEFINES)


######################################################################
####           NOTHING BELOW HERE IS REALLY CONFIGURABLE          ####
######################################################################

.SUFFIXES:	.E .sh .pl

CPP.c		= $(CC) $(CFLAGS) $(CPPFLAGS) -E
COMPILE.c	= $(CC) $(CFLAGS) $(CPPFLAGS) -c
LINK.o		= $(CC) $(LDFLAGS)
LINK.c		= $(CC) $(CFLAGS) $(CPPFLAGS) $(LDFLAGS)
OUTPUT_OPTION	= -o $@


SRC 		= article.c \
		  buffer.c \
		  connection.c \
		  endpoint.c \
		  host.c \
		  innlistener.c \
		  main.c \
		  misc.c \
		  tape.c

#  commander.c

INCS		= article.h \
		  buffer.h \
		  config.h \
		  connection.h \
		  endpoint.h \
		  host.h \
		  innlistener.h \
		  misc.h \
		  msgs.h \
		  sysconfig.h \
		  tape.h

#		  commander.h

MANSRC		= innfeed.1 innfeed.conf.5

EXTRAS		= testListener.pl inet_addr.c uio_maxiov.c \
		  procbatch.pl malloc.c ChangeLog startinnfeed.c

OBJS		= $(SRC:.c=.o) $(EXTRA_SRC:.c=.o)

ALL_GCC_WARNINGS = -pedantic -W -Waggregate-return -Wcast-align \
		   -Wcast-qual -Wcomment -Wformat \
                   -Wimplicit -Winline -Wmissing-prototypes \
                   -Wmissing-declarations -Wnested-externs \
                   -Wparentheses -Wpointer-arith \
                   -Wshadow -Wstrict-prototypes -Wswitch \
                   -Wtraditional -Wtrigraphs \
                   -Wunused -Wwrite-strings

# -Wreturn-type -Wtemplate-debugging 

MORE_WARN	= -Wconversion 

CFLAGS		= $(OSFLAGS) $(DBFLAGS) $(WARNINGS)
CPPFLAGS	= $(INCDIRS) $(DMALLOC_INCDIR) $(DMALLOC) $(DEFINES)

LOADLIBES	= $(DMALLOC_LIB) $(LIBRARIES)

VERSION		= 0.9.2
COMPRESS	= gzip
COMPRESS_EXT	= gz
TARSRC		= $(SRC) $(INCS) Makefile README INSTALL innfeed.conf \
		  $(EXTRAS) $(MANSRC) makedepend.sh
TARDIR		= innfeed-$(VERSION)
TARBALL		= $(TARDIR).tar.$(COMPRESS_EXT)


TAR		= tar
MKDIR		= mkdir
CP		= cp -p
RM		= rm -f
MV		= mv
TAG		= etags
CHMOD		= chmod

MAKEDEPEND	= ./makedepend
DEPENDFLAGS	= -s -c '$(CC) -E'

.c.o:
	$(COMPILE.c) $< $(OUTPUT_OPTION)

.c.E:
	$(CPP.c) $< $(OUTPUT_OPTION)

.sh:
	$(RM) $@ ; $(CP) $< $@ ; $(CHMOD) a-w,a+x $@

all: innfeed

innfeed: $(OBJS) version.o $(MALLOC)
	$(LINK.c) -o $@ $(OBJS) version.o $(MALLOC) $(LOADLIBES)

objs:	$(OBJS)

tags: $(SRC) $(INCS)
	$(TAG) $(SRC) $(INCS)

clean::
	$(RM) version.c *.o *.a *.core makedepend tags TAGS core \#* *~ 
	$(RM) Makefile.BAK
	$(RM) innfeed uio_maxiov listing.ps logfile lint.log
	$(RM) -r $(TARDIR)

rcsclean::
	rcsclean -u

rcscoall::
	-co -q -u RCS/*,v

realclean:: clean 
	$(RM) -r $(TARDIR)
	$(RM) $(TARBALL)

distclean:: realclean rcsclean

.PHONY:	tar listing

listing::
	@$(RM) foo.ps ; \
	F=`/bin/ls *.c *.h 2> /dev/null | sed 's/h$$/H/' |\
	   sort | sed 's/H$$/h/'` ;\
	CMD="nenscript -p- $$F | psnup -4 -d1 > listing.ps" ;\
	if [ "X$$F" != X ]; then echo $$CMD ; eval $$CMD ; fi

startinnfeed: startinnfeed.o
	$(LINK.c) -o $@ startinnfeed.o

uio_maxiov: uio_maxiov.o
	$(LINK.c) -o $@ uio_maxiov.o

check-maxiov: uio_maxiov
	@./uio_maxiov

makedepend: makedepend.sh

version.c: $(OBJS)
	echo 'const char *versionInfo = "innfeed v. $(VERSION) ('\
		`date` ')" ;' |\
		sed -e 's/( /(/' -e 's/ )/)/' > version.c

tar: $(TARBALL)

tardir: $(TARSRC)
	$(RM) -r $(TARDIR)
	$(MKDIR) $(TARDIR)
	$(CP) $(TARSRC) $(TARDIR)

$(TARBALL): tardir
	$(TAR) cvf - $(TARDIR) | $(COMPRESS) > $(TARBALL)

depend: makedepend $(SRC) $(INCS)
	$(MAKEDEPEND) $(DEPENDFLAGS) $(CPPFLAGS) $(SRC)

diffdir::
	echo -n "Directory? " ; read dir ;\
	if [ -d $$dir ]; then\
		for i in *.c *.h Makefile;do\
			[ ! -f $$dir/$$i ] && continue;\
			if ! cmp $$i $$dir/$$i > /dev/null 2>&1; then\
				echo patch $$i "<< 'EOF'";\
				diff $$dir/$$i $$i;\
				echo EOF;\
			fi;\
		done;\
	else\
		echo No such directory $$dir;\
	fi

lint:	$(OBJECTS)
	-lint -u $(LINTFLAGS) $(SRC) > lint.log

malloc.o: malloc.c
	$(COMPILE.c) -Ddebug $< $(OUTPUT_OPTION)

# DO NOT DELETE THIS LINE -- make depend depends on it.

article.o: article.c article.h buffer.h config.h endpoint.h misc.h \
   msgs.h sysconfig.h
buffer.o: buffer.c buffer.h config.h misc.h sysconfig.h
connection.o: article.h buffer.h config.h connection.c connection.h \
   endpoint.h host.h misc.h msgs.h sysconfig.h
endpoint.o: buffer.h config.h endpoint.c endpoint.h misc.h msgs.h \
   sysconfig.h
host.o: article.h config.h connection.h endpoint.h host.c host.h \
   innlistener.h misc.h msgs.h sysconfig.h tape.h
innlistener.o: article.h buffer.h config.h endpoint.h host.h \
   innlistener.c innlistener.h misc.h msgs.h sysconfig.h
main.o: article.h buffer.h config.h connection.h endpoint.h host.h \
   innlistener.h main.c misc.h msgs.h sysconfig.h tape.h
misc.o: config.h misc.c misc.h msgs.h sysconfig.h
tape.o: article.h config.h endpoint.h misc.h msgs.h sysconfig.h \
   tape.c tape.h
