#
# button maker tool
#
# Library to be linked for generating intialized structure declarations and
# top level subroutines for a button panel.
# Application create array of MenuSpec structures and call btn_SetupMenu()
# to produce button structures and subroutines to manipulate the buttons
#

#CFLAGS  = -c  -g
CFLAGS  = -c  -O

.c.o:
	$(CC) $(CFLAGS) $*.c

LIB	= libbtool.a

LIBS = ../libbtn.a

SRCS =	bitmap.c	mask.c		tool.c		rotate.c \
	text.o		wrattach.c	wrfeel.c	wrlook.c


OBJS =	bitmap.o	mask.o		tool.o		rotate.o \
	text.o		wrattach.o	wrfeel.o	wrlook.o


lib:	$(OBJS)
	ar cr $(LIB) $(OBJS)
	ranlib $(LIB)
	rm *.o

lint:
	lint -b *.c

clean:
	rm -f a.out core *.bak *~ \#* *.o


tool.o:		btnmenu.h	../buttons.h
text.o:		btnchars.h	inchars.h	outchars.h
wrattach.o:	btnmenu.h	../buttons.h
wrfeel.o:	btnmenu.h	../buttons.h
wrlook.o:	btnmenu.h	../buttons.h
