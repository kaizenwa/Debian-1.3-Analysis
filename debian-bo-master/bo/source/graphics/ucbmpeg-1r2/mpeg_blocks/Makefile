# 
# Copyright (c) 1995 The Regents of the University of California.
# All rights reserved.
# 
# Permission to use, copy, modify, and distribute this software and its
# documentation for any purpose, without fee, and without written agreement is
# hereby granted, provided that the above copyright notice and the following
# two paragraphs appear in all copies of this software.
# 
# IN NO EVENT SHALL THE UNIVERSITY OF CALIFORNIA BE LIABLE TO ANY PARTY FOR
# DIRECT, INDIRECT, SPECIAL, INCIDENTAL, OR CONSEQUENTIAL DAMAGES ARISING OUT
# OF THE USE OF THIS SOFTWARE AND ITS DOCUMENTATION, EVEN IF THE UNIVERSITY OF
# CALIFORNIA HAS BEEN ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
# 
# THE UNIVERSITY OF CALIFORNIA SPECIFICALLY DISCLAIMS ANY WARRANTIES,
# INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY
# AND FITNESS FOR A PARTICULAR PURPOSE.  THE SOFTWARE PROVIDED HEREUNDER IS
# ON AN "AS IS" BASIS, AND THE UNIVERSITY OF CALIFORNIA HAS NO OBLIGATION TO
# PROVIDE MAINTENANCE, SUPPORT, UPDATES, ENHANCEMENTS, OR MODIFICATIONS.
# 

#######################
# Name of the program #
#######################
PROGRAM = mpeg_blocks

############
# Compiler #
############
CC = gcc

#######################
# Location of *.tcl   #
#  and bitmaps dir    #
#######################

BLOCKS_DIR = \"/usr/lib/mpeg/blocks\"
#BLOCKS_DIR = \"/usr/local/etc/mpeg_blocks\"

################################
# Search path for include file #
################################
INCLUDEPATH = -I/usr/include
#INCLUDEPATH = -I/usr/include -I/usr/include/tcl 

#################################
# Search path for library file  #
#################################
LIBPATH = -L/usr/X11R6/lib

##################
# Library files  #
##################
LIBS = -ltk -ltcl -lXext -lX11 -lm 

###################
# Compiler option #
###################
OFLAGS = -O3
#OFLAGS = -g -DANALYSIS

##################
# Compiler flags #
##################
CFLAGS = $(OFLAGS) $(INCLUDEPATH) $(LIBPATH) -DBLOCKS_LIB=$(BLOCKS_DIR)

################
# Object files #
################
OBJS = show.o util.o video2.o parseblock.o motionvector.o decoders.o \
       fs2.o fs2fast.o fs4.o hybrid.o hybriderr.o 2x2.o \
       gdith2.o gray.o mono.o playframe2.o jrevdct.o 24bit.o \
       util32.o ordered.o ordered2.o mb_ordered.o monofs4.o \
       halftone.o rgbl.o big_ordered.o 2x2slow.o readfile.o 

SRCS = show.c util.c video2.c parseblock.c motionvector.c decoders.c \
       fs2.c fs2fast.c fs4.c hybrid.c hybriderr.c 2x2.c \
       gdith2.c gray.c mono.c playframe2.c jrevdct.c 24bit.c \
       util32.c ordered.c ordered2.c mb_ordered.c monofs4.c \
       halftone.c rgbl.c big_ordered.c 2x2slow.c readfile.c 

all: $(PROGRAM)

$(PROGRAM): $(OBJS)
	$(CC) $(CFLAGS) $(OBJS) $(LIBS) -o $(PROGRAM)
	strip $(PROGRAM)

clean: 
	rm -f $(OBJS) core 
	@make depend

depend:
	makedepend -- $(CFLAGS) -- $(SRCS)

clobber:
	rm -f $(OBJS) $(PROGRAM) core

# DO NOT DELETE THIS LINE -- make depend depends on it.

show.o: /usr/include/tcl.h /usr/include/stdio.h /usr/include/libio.h
show.o: /usr/include/_G_config.h /usr/include/tk.h /usr/include/X11/Xlib.h
show.o: /usr/include/sys/types.h /usr/include/linux/types.h
show.o: /usr/include/asm/types.h /usr/include/X11/X.h
show.o: /usr/include/X11/Xfuncproto.h /usr/include/X11/Xosdefs.h show.h
show.o: dither.h video.h /usr/include/setjmp.h /usr/include/features.h
show.o: /usr/include/sys/cdefs.h /usr/include/jmp_buf.h /usr/include/signal.h
show.o: /usr/include/linux/signal.h /usr/include/asm/signal.h
show.o: /usr/include/X11/Xutil.h /usr/include/sys/time.h
show.o: /usr/include/linux/time.h /usr/include/sys/time.h
show.o: /usr/include/string.h /usr/include/sys/stat.h
show.o: /usr/include/linux/stat.h
util.o: /usr/include/stdlib.h /usr/include/features.h
util.o: /usr/include/sys/cdefs.h /usr/include/errno.h
util.o: /usr/include/linux/errno.h /usr/include/asm/errno.h
util.o: /usr/include/alloca.h /usr/include/sys/types.h
util.o: /usr/include/linux/types.h /usr/include/asm/types.h
util.o: /usr/include/sys/time.h /usr/include/linux/time.h
util.o: /usr/include/sys/time.h video.h /usr/include/stdio.h
util.o: /usr/include/libio.h /usr/include/_G_config.h /usr/include/setjmp.h
util.o: /usr/include/jmp_buf.h /usr/include/signal.h
util.o: /usr/include/linux/signal.h /usr/include/asm/signal.h
util.o: /usr/include/X11/Xlib.h /usr/include/X11/X.h
util.o: /usr/include/X11/Xfuncproto.h /usr/include/X11/Xosdefs.h
util.o: /usr/include/X11/Xutil.h proto.h util.h
video2.o: /usr/include/stdio.h /usr/include/libio.h /usr/include/_G_config.h
video2.o: /usr/include/stdlib.h /usr/include/features.h
video2.o: /usr/include/sys/cdefs.h /usr/include/errno.h
video2.o: /usr/include/linux/errno.h /usr/include/asm/errno.h
video2.o: /usr/include/alloca.h /usr/include/assert.h /usr/include/sys/time.h
video2.o: /usr/include/linux/types.h /usr/include/asm/types.h
video2.o: /usr/include/linux/time.h /usr/include/sys/time.h
video2.o: /usr/include/sys/types.h decoders.h util.h video.h
video2.o: /usr/include/setjmp.h /usr/include/jmp_buf.h /usr/include/signal.h
video2.o: /usr/include/linux/signal.h /usr/include/asm/signal.h
video2.o: /usr/include/X11/Xlib.h /usr/include/X11/X.h
video2.o: /usr/include/X11/Xfuncproto.h /usr/include/X11/Xosdefs.h
video2.o: /usr/include/X11/Xutil.h proto.h /usr/include/tcl.h
video2.o: /usr/include/tk.h
parseblock.o: /usr/include/assert.h video.h /usr/include/stdio.h
parseblock.o: /usr/include/libio.h /usr/include/_G_config.h
parseblock.o: /usr/include/setjmp.h /usr/include/features.h
parseblock.o: /usr/include/sys/cdefs.h /usr/include/jmp_buf.h
parseblock.o: /usr/include/signal.h /usr/include/sys/types.h
parseblock.o: /usr/include/linux/types.h /usr/include/asm/types.h
parseblock.o: /usr/include/linux/signal.h /usr/include/asm/signal.h
parseblock.o: /usr/include/X11/Xlib.h /usr/include/X11/X.h
parseblock.o: /usr/include/X11/Xfuncproto.h /usr/include/X11/Xosdefs.h
parseblock.o: /usr/include/X11/Xutil.h proto.h decoders.h util.h
motionvector.o: video.h /usr/include/stdio.h /usr/include/libio.h
motionvector.o: /usr/include/_G_config.h /usr/include/setjmp.h
motionvector.o: /usr/include/features.h /usr/include/sys/cdefs.h
motionvector.o: /usr/include/jmp_buf.h /usr/include/signal.h
motionvector.o: /usr/include/sys/types.h /usr/include/linux/types.h
motionvector.o: /usr/include/asm/types.h /usr/include/linux/signal.h
motionvector.o: /usr/include/asm/signal.h /usr/include/X11/Xlib.h
motionvector.o: /usr/include/X11/X.h /usr/include/X11/Xfuncproto.h
motionvector.o: /usr/include/X11/Xosdefs.h /usr/include/X11/Xutil.h proto.h
motionvector.o: util.h
decoders.o: /usr/include/stdio.h /usr/include/libio.h
decoders.o: /usr/include/_G_config.h /usr/include/assert.h decoders.h util.h
decoders.o: video.h /usr/include/setjmp.h /usr/include/features.h
decoders.o: /usr/include/sys/cdefs.h /usr/include/jmp_buf.h
decoders.o: /usr/include/signal.h /usr/include/sys/types.h
decoders.o: /usr/include/linux/types.h /usr/include/asm/types.h
decoders.o: /usr/include/linux/signal.h /usr/include/asm/signal.h
decoders.o: /usr/include/X11/Xlib.h /usr/include/X11/X.h
decoders.o: /usr/include/X11/Xfuncproto.h /usr/include/X11/Xosdefs.h
decoders.o: /usr/include/X11/Xutil.h proto.h
fs2.o: video.h /usr/include/stdio.h /usr/include/libio.h
fs2.o: /usr/include/_G_config.h /usr/include/setjmp.h /usr/include/features.h
fs2.o: /usr/include/sys/cdefs.h /usr/include/jmp_buf.h /usr/include/signal.h
fs2.o: /usr/include/sys/types.h /usr/include/linux/types.h
fs2.o: /usr/include/asm/types.h /usr/include/linux/signal.h
fs2.o: /usr/include/asm/signal.h /usr/include/X11/Xlib.h /usr/include/X11/X.h
fs2.o: /usr/include/X11/Xfuncproto.h /usr/include/X11/Xosdefs.h
fs2.o: /usr/include/X11/Xutil.h dither.h fs2.h proto.h
fs2fast.o: video.h /usr/include/stdio.h /usr/include/libio.h
fs2fast.o: /usr/include/_G_config.h /usr/include/setjmp.h
fs2fast.o: /usr/include/features.h /usr/include/sys/cdefs.h
fs2fast.o: /usr/include/jmp_buf.h /usr/include/signal.h
fs2fast.o: /usr/include/sys/types.h /usr/include/linux/types.h
fs2fast.o: /usr/include/asm/types.h /usr/include/linux/signal.h
fs2fast.o: /usr/include/asm/signal.h /usr/include/X11/Xlib.h
fs2fast.o: /usr/include/X11/X.h /usr/include/X11/Xfuncproto.h
fs2fast.o: /usr/include/X11/Xosdefs.h /usr/include/X11/Xutil.h proto.h
fs2fast.o: dither.h
fs4.o: fs4.h video.h /usr/include/stdio.h /usr/include/libio.h
fs4.o: /usr/include/_G_config.h /usr/include/setjmp.h /usr/include/features.h
fs4.o: /usr/include/sys/cdefs.h /usr/include/jmp_buf.h /usr/include/signal.h
fs4.o: /usr/include/sys/types.h /usr/include/linux/types.h
fs4.o: /usr/include/asm/types.h /usr/include/linux/signal.h
fs4.o: /usr/include/asm/signal.h /usr/include/X11/Xlib.h /usr/include/X11/X.h
fs4.o: /usr/include/X11/Xfuncproto.h /usr/include/X11/Xosdefs.h
fs4.o: /usr/include/X11/Xutil.h proto.h dither.h
hybrid.o: video.h /usr/include/stdio.h /usr/include/libio.h
hybrid.o: /usr/include/_G_config.h /usr/include/setjmp.h
hybrid.o: /usr/include/features.h /usr/include/sys/cdefs.h
hybrid.o: /usr/include/jmp_buf.h /usr/include/signal.h
hybrid.o: /usr/include/sys/types.h /usr/include/linux/types.h
hybrid.o: /usr/include/asm/types.h /usr/include/linux/signal.h
hybrid.o: /usr/include/asm/signal.h /usr/include/X11/Xlib.h
hybrid.o: /usr/include/X11/X.h /usr/include/X11/Xfuncproto.h
hybrid.o: /usr/include/X11/Xosdefs.h /usr/include/X11/Xutil.h proto.h
hybrid.o: dither.h
hybriderr.o: video.h /usr/include/stdio.h /usr/include/libio.h
hybriderr.o: /usr/include/_G_config.h /usr/include/setjmp.h
hybriderr.o: /usr/include/features.h /usr/include/sys/cdefs.h
hybriderr.o: /usr/include/jmp_buf.h /usr/include/signal.h
hybriderr.o: /usr/include/sys/types.h /usr/include/linux/types.h
hybriderr.o: /usr/include/asm/types.h /usr/include/linux/signal.h
hybriderr.o: /usr/include/asm/signal.h /usr/include/X11/Xlib.h
hybriderr.o: /usr/include/X11/X.h /usr/include/X11/Xfuncproto.h
hybriderr.o: /usr/include/X11/Xosdefs.h /usr/include/X11/Xutil.h proto.h
hybriderr.o: dither.h
2x2.o: video.h /usr/include/stdio.h /usr/include/libio.h
2x2.o: /usr/include/_G_config.h /usr/include/setjmp.h /usr/include/features.h
2x2.o: /usr/include/sys/cdefs.h /usr/include/jmp_buf.h /usr/include/signal.h
2x2.o: /usr/include/sys/types.h /usr/include/linux/types.h
2x2.o: /usr/include/asm/types.h /usr/include/linux/signal.h
2x2.o: /usr/include/asm/signal.h /usr/include/X11/Xlib.h /usr/include/X11/X.h
2x2.o: /usr/include/X11/Xfuncproto.h /usr/include/X11/Xosdefs.h
2x2.o: /usr/include/X11/Xutil.h dither.h proto.h
gdith2.o: /usr/include/math.h /usr/include/features.h
gdith2.o: /usr/include/sys/cdefs.h /usr/include/huge_val.h
gdith2.o: /usr/include/endian.h /usr/include/bytesex.h /usr/include/nan.h
gdith2.o: /usr/include/values.h video.h /usr/include/stdio.h
gdith2.o: /usr/include/libio.h /usr/include/_G_config.h /usr/include/setjmp.h
gdith2.o: /usr/include/jmp_buf.h /usr/include/signal.h
gdith2.o: /usr/include/sys/types.h /usr/include/linux/types.h
gdith2.o: /usr/include/asm/types.h /usr/include/linux/signal.h
gdith2.o: /usr/include/asm/signal.h /usr/include/X11/Xlib.h
gdith2.o: /usr/include/X11/X.h /usr/include/X11/Xfuncproto.h
gdith2.o: /usr/include/X11/Xosdefs.h /usr/include/X11/Xutil.h proto.h
gdith2.o: dither.h rgbl.h show.h /usr/include/tcl.h /usr/include/tk.h
gray.o: video.h /usr/include/stdio.h /usr/include/libio.h
gray.o: /usr/include/_G_config.h /usr/include/setjmp.h
gray.o: /usr/include/features.h /usr/include/sys/cdefs.h
gray.o: /usr/include/jmp_buf.h /usr/include/signal.h /usr/include/sys/types.h
gray.o: /usr/include/linux/types.h /usr/include/asm/types.h
gray.o: /usr/include/linux/signal.h /usr/include/asm/signal.h
gray.o: /usr/include/X11/Xlib.h /usr/include/X11/X.h
gray.o: /usr/include/X11/Xfuncproto.h /usr/include/X11/Xosdefs.h
gray.o: /usr/include/X11/Xutil.h proto.h dither.h
mono.o: video.h /usr/include/stdio.h /usr/include/libio.h
mono.o: /usr/include/_G_config.h /usr/include/setjmp.h
mono.o: /usr/include/features.h /usr/include/sys/cdefs.h
mono.o: /usr/include/jmp_buf.h /usr/include/signal.h /usr/include/sys/types.h
mono.o: /usr/include/linux/types.h /usr/include/asm/types.h
mono.o: /usr/include/linux/signal.h /usr/include/asm/signal.h
mono.o: /usr/include/X11/Xlib.h /usr/include/X11/X.h
mono.o: /usr/include/X11/Xfuncproto.h /usr/include/X11/Xosdefs.h
mono.o: /usr/include/X11/Xutil.h proto.h dither.h
playframe2.o: video.h /usr/include/stdio.h /usr/include/libio.h
playframe2.o: /usr/include/_G_config.h /usr/include/setjmp.h
playframe2.o: /usr/include/features.h /usr/include/sys/cdefs.h
playframe2.o: /usr/include/jmp_buf.h /usr/include/signal.h
playframe2.o: /usr/include/sys/types.h /usr/include/linux/types.h
playframe2.o: /usr/include/asm/types.h /usr/include/linux/signal.h
playframe2.o: /usr/include/asm/signal.h /usr/include/X11/Xlib.h
playframe2.o: /usr/include/X11/X.h /usr/include/X11/Xfuncproto.h
playframe2.o: /usr/include/X11/Xosdefs.h /usr/include/X11/Xutil.h proto.h
playframe2.o: /usr/include/math.h /usr/include/huge_val.h
playframe2.o: /usr/include/endian.h /usr/include/bytesex.h /usr/include/nan.h
playframe2.o: /usr/include/values.h util.h dither.h /usr/include/tcl.h
playframe2.o: /usr/include/tk.h show.h
jrevdct.o: video.h /usr/include/stdio.h /usr/include/libio.h
jrevdct.o: /usr/include/_G_config.h /usr/include/setjmp.h
jrevdct.o: /usr/include/features.h /usr/include/sys/cdefs.h
jrevdct.o: /usr/include/jmp_buf.h /usr/include/signal.h
jrevdct.o: /usr/include/sys/types.h /usr/include/linux/types.h
jrevdct.o: /usr/include/asm/types.h /usr/include/linux/signal.h
jrevdct.o: /usr/include/asm/signal.h /usr/include/X11/Xlib.h
jrevdct.o: /usr/include/X11/X.h /usr/include/X11/Xfuncproto.h
jrevdct.o: /usr/include/X11/Xosdefs.h /usr/include/X11/Xutil.h proto.h
24bit.o: video.h /usr/include/stdio.h /usr/include/libio.h
24bit.o: /usr/include/_G_config.h /usr/include/setjmp.h
24bit.o: /usr/include/features.h /usr/include/sys/cdefs.h
24bit.o: /usr/include/jmp_buf.h /usr/include/signal.h
24bit.o: /usr/include/sys/types.h /usr/include/linux/types.h
24bit.o: /usr/include/asm/types.h /usr/include/linux/signal.h
24bit.o: /usr/include/asm/signal.h /usr/include/X11/Xlib.h
24bit.o: /usr/include/X11/X.h /usr/include/X11/Xfuncproto.h
24bit.o: /usr/include/X11/Xosdefs.h /usr/include/X11/Xutil.h dither.h proto.h
util32.o: /usr/include/stdio.h /usr/include/libio.h /usr/include/_G_config.h
util32.o: /usr/include/X11/Xlib.h /usr/include/sys/types.h
util32.o: /usr/include/linux/types.h /usr/include/asm/types.h
util32.o: /usr/include/X11/X.h /usr/include/X11/Xfuncproto.h
util32.o: /usr/include/X11/Xosdefs.h /usr/include/X11/Xutil.h video.h
util32.o: /usr/include/setjmp.h /usr/include/features.h
util32.o: /usr/include/sys/cdefs.h /usr/include/jmp_buf.h
util32.o: /usr/include/signal.h /usr/include/linux/signal.h
util32.o: /usr/include/asm/signal.h proto.h
ordered.o: video.h /usr/include/stdio.h /usr/include/libio.h
ordered.o: /usr/include/_G_config.h /usr/include/setjmp.h
ordered.o: /usr/include/features.h /usr/include/sys/cdefs.h
ordered.o: /usr/include/jmp_buf.h /usr/include/signal.h
ordered.o: /usr/include/sys/types.h /usr/include/linux/types.h
ordered.o: /usr/include/asm/types.h /usr/include/linux/signal.h
ordered.o: /usr/include/asm/signal.h /usr/include/X11/Xlib.h
ordered.o: /usr/include/X11/X.h /usr/include/X11/Xfuncproto.h
ordered.o: /usr/include/X11/Xosdefs.h /usr/include/X11/Xutil.h proto.h
ordered.o: dither.h
ordered2.o: video.h /usr/include/stdio.h /usr/include/libio.h
ordered2.o: /usr/include/_G_config.h /usr/include/setjmp.h
ordered2.o: /usr/include/features.h /usr/include/sys/cdefs.h
ordered2.o: /usr/include/jmp_buf.h /usr/include/signal.h
ordered2.o: /usr/include/sys/types.h /usr/include/linux/types.h
ordered2.o: /usr/include/asm/types.h /usr/include/linux/signal.h
ordered2.o: /usr/include/asm/signal.h /usr/include/X11/Xlib.h
ordered2.o: /usr/include/X11/X.h /usr/include/X11/Xfuncproto.h
ordered2.o: /usr/include/X11/Xosdefs.h /usr/include/X11/Xutil.h proto.h
ordered2.o: dither.h
mb_ordered.o: video.h /usr/include/stdio.h /usr/include/libio.h
mb_ordered.o: /usr/include/_G_config.h /usr/include/setjmp.h
mb_ordered.o: /usr/include/features.h /usr/include/sys/cdefs.h
mb_ordered.o: /usr/include/jmp_buf.h /usr/include/signal.h
mb_ordered.o: /usr/include/sys/types.h /usr/include/linux/types.h
mb_ordered.o: /usr/include/asm/types.h /usr/include/linux/signal.h
mb_ordered.o: /usr/include/asm/signal.h /usr/include/X11/Xlib.h
mb_ordered.o: /usr/include/X11/X.h /usr/include/X11/Xfuncproto.h
mb_ordered.o: /usr/include/X11/Xosdefs.h /usr/include/X11/Xutil.h proto.h
mb_ordered.o: dither.h
rgbl.o: video.h /usr/include/stdio.h /usr/include/libio.h
rgbl.o: /usr/include/_G_config.h /usr/include/setjmp.h
rgbl.o: /usr/include/features.h /usr/include/sys/cdefs.h
rgbl.o: /usr/include/jmp_buf.h /usr/include/signal.h /usr/include/sys/types.h
rgbl.o: /usr/include/linux/types.h /usr/include/asm/types.h
rgbl.o: /usr/include/linux/signal.h /usr/include/asm/signal.h
rgbl.o: /usr/include/X11/Xlib.h /usr/include/X11/X.h
rgbl.o: /usr/include/X11/Xfuncproto.h /usr/include/X11/Xosdefs.h
rgbl.o: /usr/include/X11/Xutil.h proto.h rgbl.h
big_ordered.o: video.h /usr/include/stdio.h /usr/include/libio.h
big_ordered.o: /usr/include/_G_config.h /usr/include/setjmp.h
big_ordered.o: /usr/include/features.h /usr/include/sys/cdefs.h
big_ordered.o: /usr/include/jmp_buf.h /usr/include/signal.h
big_ordered.o: /usr/include/sys/types.h /usr/include/linux/types.h
big_ordered.o: /usr/include/asm/types.h /usr/include/linux/signal.h
big_ordered.o: /usr/include/asm/signal.h /usr/include/X11/Xlib.h
big_ordered.o: /usr/include/X11/X.h /usr/include/X11/Xfuncproto.h
big_ordered.o: /usr/include/X11/Xosdefs.h /usr/include/X11/Xutil.h proto.h
big_ordered.o: dither.h
2x2slow.o: video.h /usr/include/stdio.h /usr/include/libio.h
2x2slow.o: /usr/include/_G_config.h /usr/include/setjmp.h
2x2slow.o: /usr/include/features.h /usr/include/sys/cdefs.h
2x2slow.o: /usr/include/jmp_buf.h /usr/include/signal.h
2x2slow.o: /usr/include/sys/types.h /usr/include/linux/types.h
2x2slow.o: /usr/include/asm/types.h /usr/include/linux/signal.h
2x2slow.o: /usr/include/asm/signal.h /usr/include/X11/Xlib.h
2x2slow.o: /usr/include/X11/X.h /usr/include/X11/Xfuncproto.h
2x2slow.o: /usr/include/X11/Xosdefs.h /usr/include/X11/Xutil.h dither.h
readfile.o: video.h /usr/include/stdio.h /usr/include/libio.h
readfile.o: /usr/include/_G_config.h /usr/include/setjmp.h
readfile.o: /usr/include/features.h /usr/include/sys/cdefs.h
readfile.o: /usr/include/jmp_buf.h /usr/include/signal.h
readfile.o: /usr/include/sys/types.h /usr/include/linux/types.h
readfile.o: /usr/include/asm/types.h /usr/include/linux/signal.h
readfile.o: /usr/include/asm/signal.h /usr/include/X11/Xlib.h
readfile.o: /usr/include/X11/X.h /usr/include/X11/Xfuncproto.h
readfile.o: /usr/include/X11/Xosdefs.h /usr/include/X11/Xutil.h proto.h
readfile.o: /usr/include/netinet/in.h /usr/include/sys/socket.h
readfile.o: /usr/include/linux/socket.h /usr/include/asm/socket.h
readfile.o: /usr/include/linux/sockios.h /usr/include/linux/uio.h
readfile.o: /usr/include/linux/in.h /usr/include/asm/byteorder.h util.h
readfile.o: dither.h
