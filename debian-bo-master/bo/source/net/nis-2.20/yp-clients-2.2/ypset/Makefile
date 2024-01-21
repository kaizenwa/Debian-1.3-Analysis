include ../Makeconfig

include ../Makerules

ifndef OPTFLAGS
OPTFLAGS= -O2
endif

CFLAGS = $(WFLAGS) $(OPTFLAGS) $(INC) $(XCFLAGS)
LDFLAGS=-s
INC= -I. -I..

OBJS  =ypset.o
PROG=ypset

all: $(PROG)

ypset: $(OBJS)
	$(CC) $(LDFLAGS) -o $@ $(OBJS) $(LIBS)

clean:
	$(RM) -f core *.o *.a *~ tmp_make foo $(PROG)
	-for i in *.c;do \
	  if [ -f $$i ]; then \
	    $(RM) -f `basename $$i .c`.s; \
	  fi; \
	done

install: $(PROG)
	install -c -o root -g root -m 0755 -s $(PROG) $(USRSBINDIR)
