

OBJS=ol_button.o ol_color.o ol_draw.o ol_init.o ol_misc.o ol_sb.o \
	ol_slider.o 

CC= gcc 
CFLAGS= -O2 -I..

all: libolgx.a

libolgx.a: $(OBJS)
	ar clq libolgx.a $(OBJS)
	ranlib libolgx.a


clean:
	rm -f *~ libolgx.a *.o

install: libolgx.a
	cp libolgx.a /usr/lib
	mkdir -p /usr/include/X11/olgx
	cp olgx.h /usr/include/X11/olgx
