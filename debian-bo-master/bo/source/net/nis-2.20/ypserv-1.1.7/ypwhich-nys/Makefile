
USRBINDIR = /usr/bin

OPTFLAGS= -O2

CFLAGS = $(WFLAGS) $(OPTFLAGS) $(INC) $(XCFLAGS)
INC= -I. -I..

OBJS  =ypwhich.o xdrfix.o
PROG=ypwhich

all: $(PROG)

ypwhich: $(OBJS)
	$(CC) $(LDFLAGS) -o $@ $(OBJS) $(LIBS)

clean:
	$(RM) -f core *.o *.a *~ tmp_make foo $(PROG)
	-for i in *.c;do \
	  if [ -f $$i ]; then \
	    $(RM) -f `basename $$i .c`.s; \
	  fi; \
	done

install: $(PROG)
	install -c -o root -g root -m 0755 -s $(PROG) $(USRBINDIR)
