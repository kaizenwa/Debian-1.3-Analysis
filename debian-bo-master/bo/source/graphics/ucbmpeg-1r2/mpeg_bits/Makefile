# Step 1:
#	Set CC to the C compiler you want to use.  On Sun, gcc 
#	produces faster code.  Your mileage may vary.
CC            = gcc
# CC            = cc -g

# Step 2:
#	Set INCLUDEDIR equal to -I followed by include directory
#	path for X11 include files. 

INCLUDEDIR    = -I/usr/include

#
# Step 3:
#	Set CFLAGS.  Be sure to change the definition of "SHOW_TCL" to point
#       to the location of show.tcl.
#       Use -DSIG_ONE_PARAM if the function passed to signal takes 
#         one parameter (linux, Solaris)
#         Do not use it if it takes none (SunOS, HPUX, Ultrix, OSF1, )
#         It works either way, (i think), just gets rid of a warning.
#
#        
#
CFLAGS	      =  $(INCLUDEDIR) -DBITS_DIR=\"/usr/lib/mpeg/bits\" -O3
#CFLAGS	      =  $(INCLUDEDIR) -DBITS_DIR=\"/usr/local/etc/mpeg_bits\" -O

#
# Step 4:
#	Set LIBS equal to path of libXext.a and libX11.a or the loader
#	flag equivalents (i.e. -lXext -lX11).  Also include tcl/tk 
#       libraries.
#       If you are NOT using shared memory, libXext.a is unecessary.
#       NOTE: below the default definition are
#             a few definitions for specific architectures.
LIBS = -L/usr/X11R6/lib -L/usr/X11/lib -ltk -ltcl -lXext -lX11 

#
# Step 5:
#	Set DEST to pathname of final destination of mpeg_bits...
#
DEST	      = ~/bin

#
# That's it!  The rest of this shouldn't need any modifications...
#

OBJS = show.o util.o video2.o parseblock.o motionvector.o decoders.o \
              	fs2.o fs2fast.o fs4.o hybrid.o hybriderr.o 2x2.o \
		gdith2.o gray.o mono.o playframe2.o jrevdct.o 24bit.o util32.o \
		ordered.o ordered2.o mb_ordered.o monofs4.o halftone.o rgbl.o \
		big_ordered.o 2x2slow.o

HDRS	      = util.h video.h decoders.h fs2.h dither.h fs4.h

INSTALL	      = /etc/install

LD	      = $(CC) 

LDFLAGS	      = 

MAKEFILE      = Makefile

PRINT	      = pr

PROGRAM       = mpeg_bits

SHELL	      = /bin/sh

SRCS	      = show.c util.c video2.c parseblock.c motionvector.c decoders.c \
              	fs2.c fs2fast.c fs4.c hybrid.c hybriderr.c 2x2.c \
		gdith2.c gray.c mono.c playframe2.c jrevdct.c 24bit.c util32.c \
		ordered.c ordered2.c mb_ordered.c monofs4.c halftone.c rgbl.c \
		big_ordered.c 2x2slow.c

SYSHDRS	      = 

all:		$(PROGRAM)

$(PROGRAM):	$(OBJS)
		$(LD) $(LDFLAGS) $(OBJS) $(LIBS) -lm -o mpeg_bits

clean:;		@rm -f *.o core mpeg_bits
 
clobber:;	@rm -f $(OBJS) $(PROGRAM) core tags

depend:;	makedepend -- $(CFLAGS) $(LIBS)-- $(SRCS)

echo:;		@echo $(HDRS) $(SRCS)

index:;		@ctags -wx $(HDRS) $(SRCS)

install:	$(PROGRAM)
		@echo Installing $(PROGRAM) in $(DEST)
		@-strip $(PROGRAM)
		@if [ $(DEST) != . ]; then \
		(rm -f $(DEST)/$(PROGRAM); $(INSTALL) -f $(DEST) $(PROGRAM)); fi

print:;		@$(PRINT) $(HDRS) $(SRCS)

tags:           $(HDRS) $(SRCS); @ctags $(HDRS) $(SRCS)

update:		$(DEST)/$(PROGRAM)

# DO NOT DELETE THIS LINE -- make depend depends on it.
