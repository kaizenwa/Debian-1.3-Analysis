#CC = gcc -Wall
CC = cc
CFLAGS = -O
#INCLUDES = -I/usr/local/include
INCLUDES = -I/util/X11/include
#LDFLAGS = -s -L/usr/local/lib
LDFLAGS = -s -L/util/X11/lib
LIBS = -lm -lX11
TARGET = xast.exe
SRCS = xast.c
OBJS = xast.o 
ALL = README xasteroids xast.docs xast.man Makefile $(SRCS) 

all:	$(TARGET)

$(TARGET):	$(OBJS)
	$(CC) $(LDFLAGS) -o $@ $(OBJS) $(LIBS)

.c.o:
	$(CC) $(CFLAGS) $(INCLUDES) -c $*.c

clean:
	rm -f $(OBJS) $(TARGET) core

shar:	$(ALL)
	shar -o $(TARGET).shar $(ALL)
