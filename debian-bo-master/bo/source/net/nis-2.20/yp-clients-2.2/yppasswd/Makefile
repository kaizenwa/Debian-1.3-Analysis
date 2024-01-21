include ../Makeconfig

include ../Makerules

# LIBS=-lshadow

ifndef OPTFLAGS
OPTFLAGS= -O2
endif

CFLAGS = $(WFLAGS) $(OPTFLAGS) $(INC) $(XCFLAGS)
INC= -I. -I..

OBJS  =yppasswd.o xdr_yppasswd.o
PROG=yppasswd

all: $(PROG)

yppasswd: $(OBJS)
	$(CC) $(LDFLAGS) -o $@ $(OBJS) $(LIBS) $(CRYPTLIB)

clean:
	$(RM) -f core *.o *.a *~ tmp_make foo $(PROG)
	-for i in *.c;do \
	  if [ -f $$i ]; then \
	    $(RM) -f `basename $$i .c`.s; \
	  fi; \
	done

install: $(PROG)
	install -c -o root -g root -m 0755 -s $(PROG) $(USRBINDIR)
