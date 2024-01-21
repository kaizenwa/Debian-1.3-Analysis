include ../../../Makefile.config

PROG=	syslogd
SRCS=	syslogd.c ttymsg.c strerror.c daemon.c
OBJ=	syslogd.o ttymsg.o strerror.o daemon.o
LIB=	reglib.a

CFLAGS=	$(COPT)

all:	syslogd logit

syslogd: $(OBJ) $(LIB)
	$(CC) $(LDFL) -o $@ $(OBJ) $(LIB) $(AUXLIB)

logit: logit.o
	$(CC) $(LDFL) -o $@ logit.o

$(LIB):
	cd reg; $(MAKE)

clean:
	rm -f syslogd logit $(OBJ) syslog.o sysloglib.a
	cd reg; $(MAKE) clean
