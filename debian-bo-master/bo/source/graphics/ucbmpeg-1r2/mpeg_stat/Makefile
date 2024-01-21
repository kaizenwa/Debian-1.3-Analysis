# Makefile for building mpeg_stat
#
# Should make just about anywhere...  If you have problems,
# check out porting.c first

CC            = gcc

# at present, no include directories needed
# Except on our OSF3.0, where -I/usr/include is needed (why? I dont know).
# On old SGIs (4.0.3 for example), add -cckr
INCLUDEDIR	      = 

# Either -g or -O (actually on some machines both is fine)

DEBUGFLAG	      = -O3

#       Use -DSIG_ONE_PARAM if the function passed to signal takes 
#         one parameter (linux, Solaris, ultrix )
#         Do not use it if it takes none (SunOS, HPUX, OSF1 )
#         It works either way, just gets rid of a warning.

CFLAGS	              =  -I/usr/include $(DEBUGFLAG) $(INCLUDEDIR)

# for SunOS cc, just use the above
# for cc on HPUX:
HP-CC-FLAGS	      =  -Aa $(CFLAGS) -D_HPUX_SOURCE -DBSD -DNONANSI_INCLUDES

HDRS	      = util.h video.h decoders.h fs2.h dither.h fs4.h

LD	      = $(CC)

OBJS          = util.o main.o decoders.o video.o jrevdct.o parseblock.o \
		motionvector.o filter.o readfile.o

PROGRAM       = mpeg_stat

SRCS	      = util.c main.c decoders.c video.c jrevdct.c parseblock.c \
		motionvector.c filter.c readfile.c

all:		$(PROGRAM)
		strip $(PROGRAM)

$(PROGRAM):	$(OBJS)
		$(LD) -g $(OBJS) $(LIBS) -lm -o $(PROGRAM)

clean:;		@rm -f *.o core $(PROGRAM)
 
