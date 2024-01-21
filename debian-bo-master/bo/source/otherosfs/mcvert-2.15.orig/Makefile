## This file is formatted with a tab-width of 3

## See the README file for hints how to configure this Makefile.

CSOURCES    = mcvert.c hqxify.c unpack.c
HSOURCES    = mactypes.h
SOURCES     = $(CSOURCES) $(HSOURCES) \
              Makefile README README-conversion mcvert.idraw mcvert.1
MANIFEST    = $(SOURCES) mcvert.man
OBJECTS     = mcvert.o hqxify.o unpack.o $(XOBJ)
ALL         = mcvert mcvert.man
EVERYTHING  = $(ALL) shar mcvert.ps
CLEAN       = $(OBJECTS) $(EVERYTHING)

VERSION=215

BIN = .

##
## *LAST* Defintion Always Wins ##
##

## Note: when using -lnet, AT&T 3B2 fails with undefined symbols like
##       t_errno first used in /usr/lib/libnet.a
##    so extract gettod.o and explicitly bind it in

### Extra Objects
# for AT&T 3B2
XOBJ= gettod.o
# common case
XOBJ=

### Bind Time Library Selection
# for SCO UNIX 3.2v4.1 with SCO TCPIP 1.2.0
LIBS= -lsocket
# for most implementations
LIBS=

## Note that ftime() is obsolete, and has been replaced by gettimeofday()
## so, in general, you should be using -DTIMEVAL
## but I am not  able  to test this  on all platforms  on all releases,
## so we leave the aging recipe in place

### Machine CFLAGS
## for machines with <timeb.h> and ftime() [and maybe without gettimeofday()]
# eg: works for ULTRIX
CFLAGS_M=
## for machines with ulong pre-defined
CFLAGS_M= -DULONG
# eg: A/UX, SGI's Irix, AT&T 3B2, IBM RS/6000's AIX
CFLAGS_M= -DTIMEVAL -DULONG
## for machines with gettimeofday() [and maybe without <timeb.h> and ftime()]
# eg: HP-UX 9.01, SunOS 4.1.3, DomainOS 10.4
CFLAGS_M= -DTIMEVAL

### Byteorder(3n) [eg: htonl()] and Byte Manipulation [eg: bzero, bcopy] CFLAGS
# uncommon case -- without support, eg: AT&T 3B2
CFLAGS_N = -DNOBYTEORDER -DNOBZEROBCOPY
# unknown case 1
CFLAGS_N = -DNOBYTEORDER
# unknown case 2
CFLAGS_N = -DNOBZEROBCOPY
# common case -- with support for  both
CFLAGS_N =

### Debugging/Optimization CFLAGS
# debugging
CFLAGS_D = -g
# optimization
CFLAGS_D = -O

### All CFLAGS
CFLAGS=$(CFLAGS_M) $(CFLAGS_N) $(CFLAGS_D) -DVERSION=$(VERSION)

### lint related
LINT=lint
LFLAGS=$(CFLAGS_M) $(CFLAGS_N) -DVERSION=$(VERSION)

all: $(ALL)
everything: $(EVERYTHING)

mcvert: $(OBJECTS)
	$(CC) $(CFLAGS) $(OBJECTS) $(LIBS) -o $(BIN)/mcvert

lint: $(HSOURCES) $(CSOURCES)
	$(LINT) $(LFLAGS) $(CSOURCES)

$(SOURCES):
	sccs get $@

gettod.o:
	ar -xv /usr/lib/libnet.a $@

$(OBJECTS): mactypes.h Makefile

shar : mcvert-$(VERSION).shar
mcvert-$(VERSION).shar : $(MANIFEST) check_linelen
	shar $(MANIFEST) > $@

# we punt the issue of expanding tabs
check_linelen: $(MANIFEST)
	nawk 'length($0) >= 80 { printf "%s: %d: ", FILENAME, FNR; print }' \
		$(MANIFEST)

clean:
	rm -f $(CLEAN)

man: mcvert.man
mcvert.man: mcvert.1
	nroff -man mcvert.1 | col | sed 's/.//g' | expand > $@
mcvert.ps: mcvert.1
	groff -man mcvert.1 > $@
