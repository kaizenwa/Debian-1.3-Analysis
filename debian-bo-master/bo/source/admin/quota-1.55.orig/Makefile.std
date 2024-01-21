PROGS         = quotacheck quotaon quota repquota warnquota quotastats edquota #rpc.rquotad 
CFLAGS       += -O6 -fexpensive-optimizations -fstrength-reduce #-DRPC -DEXT2_DIRECT
EXT2LIBS      = #-lext2fs -lcom_err
BSDLIBS       = -lbsd
RPCSRC        = rquota.h rquota_xdr.c rquota_clnt.c
VERSIONDEF    = -DQUOTA_VERSION=\"1.55\"
LDFLAGS       = -s
INSTALL       = install
ROOTDIR       =
SUPER_OWNER   = root
BIN_OWNER     = bin
BIN_GROUP     = bin
DEF_SUID_MODE = 4511
DEF_SBIN_MODE = 555
DEF_MAN_MODE  = 444

all: $(PROGS)

clean:
	-rm -f core *.o

clobber:
	-rm -f core *.o $(PROGS)

install: all 
	-$(INSTALL) -o $(BIN_OWNER) -g $(BIN_GROUP) -m $(DEF_SBIN_MODE) \
	quotacheck edquota quotaon repquota warnquota quotastats $(ROOTDIR)/usr/sbin
	$(LN) quotaon $(ROOTDIR)/usr/sbin/quotaoff
	chown $(BIN_OWNER).$(BIN_GROUP) $(ROOTDIR)/usr/sbin/quotaoff
	-$(INSTALL) -o $(BIN_OWNER) -g bin -m 644 rquota.h rquota.x $(ROOTDIR)/usr/include/rpcsvc
	-$(INSTALL) -o $(SUPER_OWNER) -g $(BIN_GROUP) -m $(DEF_SUID_MODE) quota $(ROOTDIR)/usr/ucb
	-$(INSTALL) -o $(BIN_OWNER) -g $(BIN_GROUP) -m $(DEF_MAN_MODE) *.1 $(ROOTDIR)/usr/man/man1
	-$(INSTALL) -o $(BIN_OWNER) -g $(BIN_GROUP) -m $(DEF_MAN_MODE) *.2 $(ROOTDIR)/usr/man/man2
	-$(INSTALL) -o $(BIN_OWNER) -g $(BIN_GROUP) -m $(DEF_MAN_MODE) *.3 $(ROOTDIR)/usr/man/man3
	-$(INSTALL) -o $(BIN_OWNER) -g $(BIN_GROUP) -m $(DEF_MAN_MODE) *.8 $(ROOTDIR)/usr/man/man8

quotaon: quotaon.o quotactl.o hasquota.o
	$(CC) $(LDFLAGS) -o $@ quotaon.o quotactl.o hasquota.o

quotacheck: quotacheck.o quotactl.o hasquota.o
	$(CC) $(LDFLAGS) -o $@ quotacheck.o quotactl.o hasquota.o $(EXT2LIBS)

quota: rquota.h quota.o quotactl.o rquota_clnt.o rquota_xdr.o hasquota.o
	$(CC) $(LDFLAGS) -o $@ quota.o quotactl.o rquota_clnt.o \
	rquota_xdr.o hasquota.o

repquota: repquota.o quotactl.o hasquota.o
	$(CC) $(LDFLAGS) -o $@ repquota.o quotactl.o hasquota.o

warnquota: warnquota.o quotactl.o hasquota.o
	$(CC) $(LDFLAGS) -o $@ warnquota.o quotactl.o hasquota.o

quotastats: quotastats.o quotactl.o
	$(CC) $(LDFLAGS) -o $@ quotastats.o quotactl.o

edquota: pathnames.h edquota.o quotactl.o  hasquota.o
	$(CC) $(LDFLAGS) -o $@ edquota.o quotactl.o hasquota.o

rpc.rquotad: rquota.h rquota_server.o rquota_svc.o rquota_xdr.o quotactl.o hasquota.o
	$(CC) $(LDFLAGS) -o $@ rquota_server.o rquota_svc.o rquota_xdr.o \
	quotactl.o hasquota.o $(BSDLIBS)

quota.o: quota.c
	$(CC) $(CFLAGS) $(VERSIONDEF) -c quota.c -o $@

rquota.h:
	$(RPCGEN) -h -o $@ rquota.x

rquota_xdr.c:
	$(RPCGEN) -c -o $@ rquota.x

rquota_clnt.c:
	$(RPCGEN) -l -o $@ rquota.x
