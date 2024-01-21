include ../../../Makefile.config

SRCS=	ftpd.c ftpcmd.c glob.c logwtmp.c popen.c vers.c
OBJS=	ftpd.o ftpcmd.o glob.o logwtmp.o popen.o vers.o

LIBS=	../../../libauth.a ../../../libfwall.a


# SETPROCTITLE sets the process title to something readable
# PARANOID disables many internal FTP operations
# NOEXPORT enables TIS internal export control routines
# PROXY_PASSTHROUGH enables toolkit aware proxy switching

#CFLAGS= $(COPT) -I../../.. -DNOEXPORT -DSETPROCTITLE
#CFLAGS= $(COPT) -I../../.. -DNOEXPORT -DSETPROCTITLE
CFLAGS= $(COPT) -I../../.. -DPARANOID -DSETPROCTITLE -DPROXY_PASSTHRU=\"/bin/ftp-gw\"

all: ftpd

ftpd: $(OBJS)
	$(CC) $(LDFL) -o $@ $(OBJS) $(LIBS) $(AUXLIB)

install: all
	$(CP) ftpd $(DEST)

clean:
	rm -f ftpd $(OBJS)
